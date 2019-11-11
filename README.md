# tikzr

A super simple web application for hosting `tex`-based diagrams.

## Installation

- Build using [stack](haskellstack.org) - `stack build` / `stack install` should world out of the box.
- Requires `lualatex` and `dvisvgm` on your path to work.

## Usage

- The executable `tikzr-exe` serves the application on port 8000.
- Go to `/form?ident=<name>` to edit the diagram with name `<name>`, or create it.
- Names can consist only of alphanumerical characters.
- The result of compiling with `luatex` and `dvisvgm` is visible at `/view/<name>.svg`.

## Caution

- Your diagrams are visible to absolutely everyone in the world, at least assuming they know the `<name>` you used. (This will probably never change)
- My netcode may or may not be massively unsecure - don't rely on its security!
- You cannot currently edit your diagram once you have saved a version that compiles successfully. You can view the code by going to `/view/<name>.tex` (this is awkward unless you set up your browser to open `.tex` files directly).
