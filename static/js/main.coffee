# DEVELOPMENT cache busting
requirejs.config
  urlArgs: "bust=" + (new Date()).getTime()

requirejs ["decorate", "total"], (decorator, total) ->
  $("document").ready ->
    decorator.decorateOverview()
    total.attachTo(".task:first")

