---
name: add-learning-comments
description: Add or revise teaching-oriented code comments in plain Chinese while preserving existing code logic and structure. Use when the user wants code annotated for learning, asks for line-by-line explanations, requests comments for unfamiliar functions or arguments, or wants comments to follow the style demonstrated in samplecomment.R.
---

# Add Learning Comments

Add comments that help the user understand code while keeping the code itself easy to read.

## Workflow

1. Read the complete target file and identify its role in the project.
2. Preserve code behavior, structure, naming, and execution order unless the user separately requests code changes.
3. Add a short introduction above each major file section or large function.
4. Add one or two lines of plain-language comments near unfamiliar functions, important arguments, conditions, and returned values.
5. For nested, reactive, event-driven, or cross-file code, explain the local data flow: where the input comes from, what value changes, and which later result changes because of it.
6. Replace user-written question comments such as “啥意思”, “为什么要用”, or “怎么触发” with direct answers instead of merely deleting them.
7. Read the annotated code again and remove comments that repeat obvious code or make the file harder to follow.
8. Run the language's syntax or parse check after editing when one is available.

## Comment Style

- Write comments in Chinese或者中英结合 unless the user requests another language.
- Explain technical terms with everyday wording. Prefer “项目文件夹的完整路径” over “绝对路径字符串”.
- Above a large function, explain:
  - what the function does;
  - its role in the overall project;
  - important inputs or return values only when they exist and matter.
- Near individual code, explain:
  - what an unfamiliar function call does here;
  - what important arguments change;
  - why a condition or fallback exists;
  - what the resulting value represents.
- For a function used inside loops, nested UI builders, reactive expressions, or event handlers, also explain its role in that structure. For example, explain what an outer `lapply()` repeats and what an inner `lapply()` creates.
- Explain event-driven terms through concrete cause and effect. Prefer “点击 VAR 后，`input$method_link_var` 变化，于是运行 `open_method("var")`” over “the event is observed”.
- Explain reactive code by naming the before value, after value, and downstream result when practical. For example, `selected_method(): "linear_regression" -> "var"` causes the title, case, plots, and tables to refresh.
- When several files connect, explain the handoff at the later use site: which function was loaded, what input is passed into it, what it returns, and who uses that return value next.
- Keep each local explanation to one or two comment lines where practical.
- Use a concrete command, path, or value example when it makes an abstract idea easier to understand.
- Explain the current code in context, rather than giving a general textbook definition.
- Group closely related calls into one explanation when separate comments would become repetitive.
- Do not comment every obvious assignment, closing bracket, or common operator.
- Do not place long comments at the end of code lines. Put explanations immediately above the related code.

## Plain-Language Requirements

- Avoid unexplained jargon such as “调用栈”, “命名空间”, or “绝对路径字符串”.
- If a technical term is necessary, immediately explain what it means in this code.
- Identify where custom functions come from when that is not obvious, for example: “这个函数来自 R/packages.R”.
- Explain fallback values such as `NULL`, `FALSE`, or an empty result in terms of what they mean to the user.
- Clearly distinguish similar operations, such as the current folder from a child folder appended with `file.path()`.
- Translate framework words into visible behavior:
  - “listen/observe” means waiting for a specific user action or value change, then running named code;
  - “active style” means the visible CSS appearance of the currently selected item;
  - “session” means the current browser connection;
  - “tag” means an HTML page element created from R.
- For unfamiliar arguments, explain the effect of the actual value used in this code, not every possible value supported by the function.

## Boundaries

- Do not reorganize or refactor code merely to make it easier to comment.
- Do not change behavior while performing a comments-only request.
- Do not add comments that claim more than the code guarantees.
- Preserve useful existing comments and revise only comments that are unclear, inaccurate, too technical, or too long.

## Reference

For the approved R comment style, read [references/r-comment-style-example.R](references/r-comment-style-example.R).
Use it as a style reference, not as code to copy blindly.

For nested Shiny UI, reactive values, events, sessions, and cross-file data flow, also read
[references/r-shiny-structural-comment-example.R](references/r-shiny-structural-comment-example.R).
