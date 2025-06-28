# UUID Generator for Emacs

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/insert-uuid-badge.svg)](https://melpa.org/#/insert-uuid)
[![MELPA Stable](https://stable.melpa.org/packages/insert-uuid-badge.svg)](https://stable.melpa.org/#/insert-uuid)
[![Emacs](https://img.shields.io/badge/Emacs-27.1%2B-blueviolet.svg)](https://www.gnu.org/software/emacs/)

A comprehensive UUID (Universally Unique Identifier) generator for GNU Emacs that fully implements the RFC 4122 standard.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [API Reference](#api-reference)
- [Customization](#customization)
- [UUID Types](#uuid-types)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

## Features

- 🎯 **Full RFC 4122 Compliance** - Properly sets version and variant bits according to the standard
- 🔢 **Multiple UUID Versions** - Supports versions 1, 3, 4, and 5
- 🏷️ **Predefined Namespaces** - DNS, URL, OID, and X.500 namespaces included
- ⚡ **Interactive Commands** - Easy UUID insertion with `M-x` commands
- 🎨 **Customizable** - Configure default version and uppercase/lowercase output
- 📦 **Zero Dependencies** - Uses only built-in Emacs functionality
- 🚀 **Fast & Efficient** - Optimized for performance

## Installation

### Using MELPA

```elisp
;; Add MELPA to your package archives if you haven't already
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install insert-uuid
M-x package-install RET insert-uuid RET
```

### Using use-package

```elisp
(use-package insert-uuid
  :ensure t
  :bind (("C-c u" . insert-uuid)
         ("C-c U" . insert-uuid-random))
  :custom
  (insert-uuid-default-version 4)
  (insert-uuid-uppercase nil))
```

### Manual Installation

1. Clone this repository or download `insert-uuid.el`
2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/insert-uuid")
(require 'insert-uuid)

;; Optional: Set up key bindings
(global-set-key (kbd "C-c u") 'insert-uuid)
```

## Usage

### Interactive Commands

```elisp
;; Insert a UUID (prompts for version)
M-x insert-uuid

;; Insert a random UUID (version 4) quickly
M-x insert-uuid-random
```

### Programmatic Usage

```elisp
;; Generate a random UUID (version 4)
(insert-uuid-create 4)
;; => "550e8400-e29b-4d4a-a716-446655440000"

;; Generate a time-based UUID (version 1)
(insert-uuid-create 1)
;; => "2d6a2e3a-abcd-11ef-8123-0123456789ab"

;; Generate a name-based UUID using MD5 (version 3)
(insert-uuid-create 3 'dns "www.example.com")
;; => "5df41881-3aed-3515-88a7-2f4a814cf09e"

;; Generate a name-based UUID using SHA-1 (version 5)
(insert-uuid-create 5 'url "https://github.com/user/repo")
;; => "2ed6657d-e927-568b-95e1-2665a8aea6a2"

;; Use a custom namespace
(insert-uuid-create 5 "6ba7b810-9dad-11d1-80b4-00c04fd430c8" "custom-name")
;; => "88f3c7a0-4b2a-5439-9f53-4537cd01cfd0"
```

## API Reference

### Functions

#### `insert-uuid-create (version &optional namespace name)`

Generate a UUID according to RFC 4122.

- **version** - UUID version (1, 3, 4, or 5)
- **namespace** - (Optional) For versions 3 and 5: UUID string or symbol (`'dns`, `'url`, `'oid`, `'x500`)
- **name** - (Optional) For versions 3 and 5: Name string to hash

#### `insert-uuid (version)`

Interactively insert a UUID at point. Prompts for version and additional parameters as needed.

#### `insert-uuid-random ()`

Insert a random UUID (version 4) at point without prompting.

### Variables

#### `insert-uuid-default-version`

Default UUID version to use when not specified. (Default: 4)

#### `insert-uuid-uppercase`

If non-nil, generate UUIDs in uppercase. (Default: nil)

## Customization

You can customize the package behavior through Emacs' customize interface:

```
M-x customize-group RET insert-uuid RET
```

Or set variables directly in your configuration:

```elisp
;; Use version 5 as default
(setq insert-uuid-default-version 5)

;; Generate uppercase UUIDs
(setq insert-uuid-uppercase t)
```

## UUID Types

### Version 1: Time-based

Generated using timestamp and MAC address (uses random multicast address for privacy).

```elisp
(insert-uuid-create 1)
;; => "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
```

### Version 3: Name-based (MD5)

Generated using MD5 hash of namespace and name.

```elisp
(insert-uuid-create 3 'dns "example.com")
;; => "9073926b-929f-31c2-abc9-fad77ae3e8eb"
```

### Version 4: Random

Generated using random or pseudo-random numbers.

```elisp
(insert-uuid-create 4)
;; => "f47ac10b-58cc-4372-a567-0e02b2c3d479"
```

### Version 5: Name-based (SHA-1)

Generated using SHA-1 hash of namespace and name.

```elisp
(insert-uuid-create 5 'url "https://example.com")
;; => "2ed6657d-e927-568b-95e1-2665a8aea6a2"
```

## Examples

### Generate UUIDs for Git Commits

```elisp
(defun my/insert-commit-id ()
  "Insert a UUID suitable for a commit identifier."
  (interactive)
  (insert "Commit-ID: " (insert-uuid-create 4)))
```

### Generate Consistent IDs for Resources

```elisp
;; Always generates the same UUID for the same URL
(defun my/url-to-uuid (url)
  "Convert a URL to a consistent UUID."
  (insert-uuid-create 5 'url url))

(my/url-to-uuid "https://github.com/emacs-mirror/emacs")
;; Always returns: "4c104dd0-4821-50d7-9c9a-0f2e26c8b7aa"
```

### Batch Generate UUIDs

```elisp
(defun my/generate-uuid-list (count &optional version)
  "Generate COUNT UUIDs of the specified VERSION (default 4)."
  (cl-loop repeat count
           collect (insert-uuid-create (or version 4))))

;; Generate 5 random UUIDs
(my/generate-uuid-list 5)
```

### UUID-based Database IDs

```elisp
(defun my/create-record (table-name data)
  "Create a record with a UUID primary key."
  (let ((id (insert-uuid-create 4)))
    (message "Creating %s record with ID: %s" table-name id)
    ;; Your database insertion code here
    id))
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Frequently Asked Questions

### Q: Why doesn't version 1 use my real MAC address?

**A:** For privacy reasons, this implementation uses a random multicast MAC address instead of reading the actual network interface address.

### Q: Can I use this package to validate existing UUIDs?

**A:** This package is focused on generation. For validation, you might want to combine it with a regex pattern:

```elisp
(defun my/valid-uuid-p (string)
  "Return t if STRING is a valid UUID."
  (string-match-p
   "^[0-9a-fA-F]\\{8\\}-[0-9a-fA-F]\\{4\\}-[1-5][0-9a-fA-F]\\{3\\}-[89abAB][0-9a-fA-F]\\{3\\}-[0-9a-fA-F]\\{12\\}$"
   string))
```

### Q: Is this package available on MELPA?

**A:** Not yet, but we're working on it! For now, please use manual installation.

## Acknowledgments

- RFC 4122 authors for the comprehensive UUID specification
- The Emacs community for continuous support and feedback

## License

This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version.

See the [LICENSE](LICENSE) file for details.

---

Made with ❤️ for the Emacs community