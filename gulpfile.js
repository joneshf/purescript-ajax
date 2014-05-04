var gulp = require('gulp')
  , clean = require('gulp-clean')
  , purescript = require('gulp-purescript')
  , util = require('gulp-util');

paths = {
    src: [
        'src/**/*.purs',
        '!bower_components/purescript-quickcheck/**/*.purs',
        'bower_components/purescript-*/src/**/*.purs'
    ],
    test: [
        'src/**/*.purs',
        'test/**/*.purs',
        'bower_components/purescript-*/src/**/*.purs'
    ]
}

gulp.task('purescript', function() {
    var psc = purescript.psc();
    psc.on('error', function(e) {
        util.log(e);
        psc.end();
    });
    return gulp.src(paths.src)
      .pipe(psc)
      .pipe(gulp.dest('js'));
});

gulp.task('test-clean', function() {
    return gulp.src('test')
      .pipe(clean());
});

gulp.task('test-compile', ['test-clean'], function() {
    return gulp.src(paths.test)
      .pipe(purescript.psc())
      .pipe(gulp.dest('test'));;
});

gulp.task('watch', function() {
    gulp.watch(paths.src, ['purescript']);
});

gulp.task('default',['purescript', 'watch']);
gulp.task('test',['test-clean', 'test-compile']);
