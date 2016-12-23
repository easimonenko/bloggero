const gulp = require('gulp'),
  commander = require('commander'),
  connect = require('gulp-connect'),
  elm = require('gulp-elm'),
  gutil = require('gulp-util'),
  plumber = require('gulp-plumber')

commander
  .option('--output <path>', 'path to buit application', 'sample/app.js')
  .option('--dir <path>','path to sources of blog', 'sample')
  .option('--debug', '--debug option to elm-make')
  .parse(process.argv)

const
  output = commander.output,
  dir = commander.dir,
  debug = commander.debug || false

gutil.log('output = ' + output)
gutil.log('dir = ' + dir)
gutil.log('debug = ' + debug)

gulp.task('elm-init', elm.init)

gulp.task('build', ['elm-init'], () => {
  return gulp.src('src/**/*.elm')
    .pipe(plumber())
    .pipe(elm.bundle(output, {debug: debug}))
    .pipe(gulp.dest('.'))
})

gulp.task('server', [], () => {
  connect.server({
    port: 8000,
    root: dir,
    livereload: true,
    debug: true
  })
})

gulp.task('watch', [], () => {
  gulp.watch('src/**/*.elm', ['build'])

  const watchlist = [
    '/index.html',
    '/css/**/*.css',
    '/config.json'
  ].map((path) => {
    return dir + path
  })
  watchlist.unshift(output)

  gulp.watch(watchlist, (event) => {
    gutil.log('LiveReload ' + event.path)

    return gulp.src(event.path)
      .pipe(plumber())
      .pipe(connect.reload())
  })
})

gulp.task('default', ['watch', 'server'])
