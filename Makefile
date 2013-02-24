TEMPDIR=build-tmp

package: dist/build/timetracker/timetracker
	coffee -c static/js
	r.js -o build.js out=static/js/main.built.js
	mkdir -p $(TEMPDIR)
	cp dist/build/timetracker/timetracker $(TEMPDIR)
	cp -r config $(TEMPDIR)
	mkdir -p $(TEMPDIR)/static/js
	cp -r static/font $(TEMPDIR)/static
	cp -r static/img $(TEMPDIR)/static
	cp -r static/js/main.built.js $(TEMPDIR)/static/js
	cp -r static/js/jquery.min.js $(TEMPDIR)/static/js
	cp -r static/js/require.js $(TEMPDIR)/static/js
	(cd $(TEMPDIR) && tar cvzf ../timetracker.tar.gz *)
	rm -rf $(TEMPDIR)
	rm static/js/main.built.js

clean:
	cabal clean

dist/build/timetracker/timetracker:
	sed -i -e "s/main\\.js/main.built.js/" templates/default-layout-wrapper.hamlet
	cabal clean
	cabal configure
	cabal build
	sed -i -e "s/main\\.built\\.js/main.js/" templates/default-layout-wrapper.hamlet

