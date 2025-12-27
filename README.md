# .emacs.d

This is my personal Emacs configuration.

## Features

- **Package-free `lite` branch**: No external packages installed, works on legacy systems and pure TTY environments
- **Left-handed keybindings**: Repurposes `M-q`, `M-z`, `M-e`, `C-q`, `C-z` for efficient left-hand-only workflows
- **Version compatibility**: Graceful degradation for Emacs 24.5+ (tested primarily on Arch Linux)

## Installation

```bash
git clone https://github.com/OChicken/.emacs.d.git $HOME/.emacs.d
git clone https://github.com/OChicken/.emacs.d.git -b lite $HOME/.emacs.d  # lite version
```

## On using lite (package-free) Emacs

``` bash
emacs --lite [OPTION-OR-FILENAME]...
```

## Key bindings

The configuration heavily modifies default Emacs bindings for left-hand operation:

- `M-q` / `C-q` → Navigate up (↑)
- `M-z` / `C-z` → Navigate down (↓)
- `M-e` → Enter/Return (⏎)
- `M-a` → Beginning of line
- `C-a` → Back to indentation

**Note**: `C-z` (suspend-frame) is still available via `C-x C-z`.

## Compatibility

Primarily tested on **Arch Linux**. Some features may not be available on older Ubuntu LTS releases due to legacy Emacs versions, though the configuration includes graceful degradation where possible.

Use at your own risk.
