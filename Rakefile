require "fileutils"

tmp_dir = "/tmp/timetracker-build"
original_dir = Dir.pwd

task :default => :pack_update

task :build_execuatable do
  sh 'sed -i -e "s/main\\.js/main.built.js/" templates/default-layout-wrapper.hamlet'
  sh "cabal clean"
  sh "cabal configure"
  sh "cabal build"
  sh 'sed -i -e "s/main\\.built\\.js/main.js/" templates/default-layout-wrapper.hamlet'
end

task :build_env do
  sh "mkdir -p #{tmp_dir}"
  sh "cp -r dist/build/timetracker/timetracker build.js config static #{tmp_dir}"
  Dir.chdir(tmp_dir)
end

task :build_coffee do
  sh "coffee -c static/js"
end

task :build_rjs do
  sh "uglifyjs static/js/components/requirejs/require.js > static/js/require.min.js"
  sh "r.js -o build.js out=static/js/main.built.js"
end

task :pack do
  sh "tar czvf timetracker.tar.gz *"
  sh "mv timetracker.tar.gz \"#{original_dir}\""
end

task :cleanup_assets do
  to_delete = [
    "static/tmp",
    "static/js/components",
    "config/models",
    "config/routes",
    "config/favicon.ico",
    "config/keter.yaml",
    "config/client_session_key.aes",
    "config/robots.txt",
    "config/*.aes",
    "build.js"
  ]
  preserve = [
    "main.built.js",
    "jquery.min.js",
    "require.min.js"
  ]
  FileUtils.rm_rf(to_delete)
  js_lint = Dir["static/js/*"].reject { |f| preserve.include? File.basename(f) }
  FileUtils.rm_rf(js_lint)
end

task :cleanup_tmp_dir do
  Dir.chdir(original_dir)
  sh "rm -rf \"#{tmp_dir}\""
end

task :cleanup_config do
  sh "rm -rf config"
end

task :pack_init => [:build_execuatable, :build_env, :build_coffee, :build_rjs,
                    :cleanup_assets, :pack, :cleanup_tmp_dir]

task :pack_update => [:build_execuatable, :build_env, :build_coffee, :build_rjs,
                      :cleanup_assets, :pack, :cleanup_tmp_dir]

task :pack_update_nobuild => [:build_env, :build_coffee, :build_rjs,
                              :cleanup_assets, :cleanup_config, :pack,
                              :cleanup_tmp_dir]
