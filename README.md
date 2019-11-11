# tikzr

A super simple web application for hosting `tex`-based diagrams.

- Go to `/form?ident=<name>` to create a diagram with name `<name>`
- Names can consist only of alphanumerical characters.
- The result of compiling with `luatex` and `dvisvgm` is visible at `/view/<name>.svg`.
- There's a link on `/` to create a randomly-named diagram.
    - There's a tiny risk of collisions, which I deal with by ignoring it. Just reload `/` and generate a new one.

## Caution

- Your diagrams are visible to absolutely everyone in the world, at least assuming they know the `<name>` you used. (This will probably never change)
- My netcode may or may not be massively unsecure - don't rely on its security!
- You cannot currently edit your diagram once you have saved a version that compiles successfully. You can view the code by going to `/view/<name>.tex` (this is awkward unless you set up your browser to open `.tex` files directly).
