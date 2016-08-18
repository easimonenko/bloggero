# bloggero-elm-mdl
Bloggero -- engine for static blogs with single page interface written in Elm
with Material Design Light.

## First

I recommend that you first fork this repository in order to be able to update
Your blog application from these sources.

## Build

``` sh
npm run build
```

So will downloaded the required packages are Elm and builded the application
from source.

## Using

Once setup, you can edit the blog page as plain text files. Supported formats
are Markdown and HTML.

Each page of the blog is a subdirectory of the blog directory, which should
contain two files:

* `index.json` -- page description
* `index.markdown` или `index.html` -- page content

This folder can also contain other content, such as images.

For non-static pages should be written the appropriate code for Elm in the
`src` directory. You can also link to your pages ready Javascript.

Run a static web server:

``` sh
npm start
```

Please note that Elm required packages are installed automatically, but You
can install them yourself:

``` sh
npm install -g elm
elm package install
```

## Development

For automated build and reload the application, you can use `elm-live`:

``` sh
npm start
```

If You plan to develop in the environment such as Atom editor, You need to first
install Elm globally.

``` sh
npm install -g elm
```

## Customizing

To configure the blog, edit the `config.json`. Pay special attention to what is
in the directory `sample` is a blog template. For your blog, copy those files
to a new directory, and then edit the `config.json`.

## Technologies

The project is written in Elm version 0.17 using the elm-mdl, which implements
on Elm user interface Material Design Lite.

---

(c) Evgeniy A. Simonenko <easimonenko@mail.ru>, 2016
