# dot-env.el

dot-env.el makes it easy to keep track of emacs configuration variables across multiple environments and machines. It aims to mimic the functionality found in [https://github.com/motdotla/dotenv](https://github.com/motdotla/dotenv), allowing you to specify values for config items that are likely to change across machines.

Examples of such config items are font sizes, paths to executables such as `ispell`, and authentication tokens.

## Usage
```elisp
(setq dot-env-filepath "path to your .env file")
;; defaults to `.env` in your emacs config directory
```

```elisp
(dot-env-load)
;; loads your .env file into the emacs environment
;; should be called at the beginning of your emacs configuration
```

```elisp
(dot-env-get "VARIABLE_NAME" "default value")
;; returns a value from your .env file.
;; If no such variable exists, uses "default value".
```

## Example: setting font
Value in .env file
```bash
FONT="DejaVu Sans Mono-11"
```

Emacs config
```elisp
;; load the .env file
(dot-env-load)

;; somewhere in your emacs config
(set-frame-font (dot-env-get "FONT" "DejaVu Sans Mono-13") nil t)
```
The above code changes the font to `DejaVu Sans Mono-11`. If no value had been specified in the .env file, `DejaVu Sans Mono-13` would have been loaded instead.

## Tips
* It's easiest just to use the default name and location for the .env file - `.env` in your `.emacs.d` directory.
* You should add the name/path of your .env file to your `.gitignore` file.

## Disclaimer
This package doesn't support the full dotenv spec but should work for most use cases. It filters out commented lines (prefixed with a `#`), and single and double quotes. Multiline values are not supported.
