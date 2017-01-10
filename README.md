# bloggero-elm-mdl

Bloggero -- engine for static blogs with a single page interface written in Elm
with Material Design Light.

## Download and start with Elm

[Guide and download link for Elm](https://guide.elm-lang.org/get_started.html)

If You plan to develop in an environment such as the Atom editor, you should
install Elm globally.

``` sh
npm install -g elm
```

## First

I recommend that you first fork or clone this repository in order to be able to
update your blog application from these sources.

## Build

Go into the downloaded directory then:

``` sh
npm run build -- --output=the_path_to_your_blog/app.js
```

This will download the required packages for Elm and build the application
from source.

If You want to build for sample a blog, run:

``` sh
npm run build -- --output=sample/app.js
```

## Usage

Once setup, you can edit the blog page as plain text files. Supported formats
are Markdown and HTML.

Each page of the blog is a subdirectory of the blog directory, which should
contain two files:

* `index.json` -- page description
* `index.markdown` or `index.html` -- page content

This folder can also contain other content, such as images.

For non-static pages write the appropriate code for Elm in the
`src` directory. You can also link to Javascript.

Run a static web server:

``` sh
npm start -- --output=the_path_to_your_blog/app.js --dir=the_path_to_your_blog
```

If You want to see for sample a blog, run:

``` sh
npm start -- --output=sample/app.js --dir=sample
```

If You want to your blog automatically shows up in the browser, add the option
`--open`.

Please note that the packages required by Elm are installed automatically, but
you can install them yourself:

``` sh
npm install -g elm
elm package install
```

## Development

For automated building and reloading of the application, you can use
[elm-live](https://github.com/tomekwi/elm-live):

``` sh
npm start -- --output=the_path_to_your_blog/app.js --dir=the_path_to_your_blog
```

## Customizing

Pay special attention to what is in the directory `sample` is a example
template. For your blog, copy those files to a new directory, and then edit the
`config.json` in appropriate directory to configure the blog/website.

## Alternative usage with Gulp

If You want to see for sample a blog, run:

``` sh
gulp
```

This sets by default options:

* --output sample/app.js
* --dir sample

Full command:

``` sh
gulp --output=the_path_to_your_blog/app.js --dir=the_path_to_your_blog
```

Use --debug option for enable debug history. By default false.

## Technologies

The project is written in Elm version 0.18 using the elm-mdl, which implements
on Elm user interface Material Design Lite.

## Our users

See [USERS.md](USERS.md).

---

(c) Evgeniy A. Simonenko <easimonenko@mail.ru>, 2016-2017
