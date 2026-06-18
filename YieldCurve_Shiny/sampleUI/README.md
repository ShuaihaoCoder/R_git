# YieldCurve UI v3 Reference Screens

This folder stores the scheme-two product-design screenshots used as the visual reference for the Shiny dashboard.

## Mapping

- `00_scheme2_overview.png`: overall dense institutional trader dashboard direction.
- `01_curve_explorer.png`: Curve Explorer page reference.
- `02_history_changes.png`: History & Changes page reference.
- `03_forward_calculator.png`: Forward Calculator page reference.
- `04_carry_roll.png`: Carry & Roll page reference.
- `05_diagnostics.png`: Diagnostics page reference.

## Implementation Rule

The Shiny UI should keep these visual modules visible. If a module cannot be supported by the current local RDS or calculation engine, keep the card in place and show an unavailable message rather than hiding it.
