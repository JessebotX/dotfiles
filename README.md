# dotfiles

Dotfiles are plain-text based files that store preferences and settings made to
a program. They are usually prefixed with a dot (`.`) which signifies that they
are hidden files (on Unix-like systems).

This is my repository of configurations for programs I use often.

## Deployment on Linux
1. Install GNU Stow
2. Using the provided `deploy` script, run the following command to deploy one
   of the folders ("programs")
   
   ```bash
   ./deploy <folder name> # eg ./deploy emacs
   ```
