param(
  [int]$AppPort = 7855,
  [int]$DebugPort = 9224,
  [string]$OutPath = "C:\Users\PC\Desktop\R_git\YieldCurve_Shiny\current_carry_roll_applied.png"
)

$ErrorActionPreference = "Stop"

$chrome = "C:\Program Files\Google\Chrome\Application\chrome.exe"
$edge = "C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe"
$browser = if (Test-Path $chrome) { $chrome } elseif (Test-Path $edge) { $edge } else { throw "No Chrome/Edge browser found." }
$profile = Join-Path $PSScriptRoot "..\edge-cdp-profile"
$profile = [System.IO.Path]::GetFullPath($profile)

$args = @(
  "--headless=new",
  "--disable-gpu",
  "--no-first-run",
  "--disable-background-networking",
  "--remote-allow-origins=*",
  "--remote-debugging-port=$DebugPort",
  "--user-data-dir=$profile",
  "--window-size=1512,982",
  "about:blank"
)

$proc = Start-Process -FilePath $browser -ArgumentList $args -WindowStyle Hidden -PassThru

function Wait-ForJson($uri, $timeoutSeconds = 20) {
  $deadline = (Get-Date).AddSeconds($timeoutSeconds)
  do {
    try { return Invoke-RestMethod -Uri $uri -UseBasicParsing -TimeoutSec 2 } catch { Start-Sleep -Milliseconds 300 }
  } while ((Get-Date) -lt $deadline)
  throw "Timed out waiting for $uri"
}

function Send-Cdp($ws, [int]$id, [string]$method, $params = $null) {
  $payload = @{ id = $id; method = $method }
  if ($null -ne $params) { $payload.params = $params }
  $json = $payload | ConvertTo-Json -Depth 20 -Compress
  $bytes = [System.Text.Encoding]::UTF8.GetBytes($json)
  $segment = [ArraySegment[byte]]::new($bytes)
  $ws.SendAsync($segment, [System.Net.WebSockets.WebSocketMessageType]::Text, $true, [Threading.CancellationToken]::None).GetAwaiter().GetResult()
}

function Receive-Cdp($ws) {
  $buffer = New-Object byte[] 1048576
  $chunks = New-Object System.Collections.Generic.List[byte]
  do {
    $segment = [ArraySegment[byte]]::new($buffer)
    $result = $ws.ReceiveAsync($segment, [Threading.CancellationToken]::None).GetAwaiter().GetResult()
    if ($result.Count -gt 0) {
      for ($i = 0; $i -lt $result.Count; $i++) { $chunks.Add($buffer[$i]) }
    }
  } until ($result.EndOfMessage)
  [System.Text.Encoding]::UTF8.GetString($chunks.ToArray()) | ConvertFrom-Json
}

function Invoke-Cdp($ws, [ref]$nextId, [string]$method, $params = $null) {
  $id = $nextId.Value
  $nextId.Value = $nextId.Value + 1
  Send-Cdp $ws $id $method $params
  while ($true) {
    $message = Receive-Cdp $ws
    if ($message.id -eq $id) { return $message }
  }
}

try {
  Wait-ForJson "http://127.0.0.1:$DebugPort/json/version" | Out-Null
  $appUrl = "http://127.0.0.1:$AppPort"
  $target = Invoke-RestMethod -Method Put -Uri ("http://127.0.0.1:$DebugPort/json/new?" + [System.Uri]::EscapeDataString($appUrl)) -UseBasicParsing

  $wsUrl = [string]$target.webSocketDebuggerUrl
  $wsUrl = $wsUrl.Replace("ws://localhost:", "ws://127.0.0.1:")
  $ws = [System.Net.WebSockets.ClientWebSocket]::new()
  $ws.ConnectAsync([Uri]$wsUrl, [Threading.CancellationToken]::None).GetAwaiter().GetResult()
  $nextId = 1

  Invoke-Cdp $ws ([ref]$nextId) "Page.enable" | Out-Null
  Invoke-Cdp $ws ([ref]$nextId) "Runtime.enable" | Out-Null
  Start-Sleep -Seconds 5

  $script = @"
(async () => {
  const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
  const click = selector => {
    const el = document.querySelector(selector);
    if (!el) throw new Error('Missing selector: ' + selector);
    el.click();
  };
  click('a[data-value="Carry & Roll"]');
  await sleep(1500);
  click('#calculate_carry');
  await sleep(3500);
  click('#calculate_curve_trade');
  await sleep(4500);
  window.scrollTo(0, 0);
  await sleep(800);
  return {
    activeTab: document.querySelector('.nav-link.active')?.textContent?.trim(),
    title: document.querySelector('.tab-pane.active h2')?.textContent?.trim(),
    carry: document.querySelector('#carry_value')?.textContent?.trim(),
    trade: document.querySelector('#trade_total_pnl')?.textContent?.trim()
  };
})()
"@
  $eval = Invoke-Cdp $ws ([ref]$nextId) "Runtime.evaluate" @{
    expression = $script
    awaitPromise = $true
    returnByValue = $true
  }
  $status = $eval.result.result.value
  if ($status.activeTab -ne "Carry & Roll") {
    throw "Expected Carry & Roll tab, got '$($status.activeTab)'."
  }

  $shot = Invoke-Cdp $ws ([ref]$nextId) "Page.captureScreenshot" @{
    format = "png"
    captureBeyondViewport = $false
    fromSurface = $true
  }
  $bytes = [Convert]::FromBase64String($shot.result.data)
  [System.IO.File]::WriteAllBytes($OutPath, $bytes)
  $status | ConvertTo-Json -Compress
}
finally {
  if ($null -ne $ws) { $ws.Dispose() }
  if ($null -ne $proc -and -not $proc.HasExited) { Stop-Process -Id $proc.Id -Force }
}
