module.exports = function(grunt) {
  require("jit-grunt")(grunt);

  grunt.initConfig({
    less: {
      development: {
        options: {
          compress: true,
          yuicompress: true,
          optimization: 2
        },
        files: {
          "../inst/www/miniUI.css": "../less/gadgets.less" // destination file and source file
        }
      }
    },
    postcss: {
        options: {
            map: false,
            processors: [
                require("autoprefixer")({
                    browsers: ["last 2 versions"]
                })
            ]
        },
        dist: {
            src: "../inst/www/miniUI.css"
        }
    },
    watch: {
      styles: {
        files: ["less/**/*.less"], // which files to watch
        tasks: ["less", "postcss"],
        options: {
          nospawn: true
        }
      }
    }
  });

  grunt.registerTask("default", ["less", "postcss"]);
};
