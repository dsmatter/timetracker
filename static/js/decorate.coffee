define ->
  decorateOverview: ->
    @setupHoverFor($("body"))
    @setupTasksHover()

  setupTasksHover: ->
    $(".task").hover \
    (->
      $(@).find(".sessions").removeClass("hidden")),
    (->
      $(@).find(".sessions").addClass("hidden"))

  setupHoverFor: (element) ->
    self = @
    element.hover \
    (->
      $(@).addClass "selected"),
    (->
      $(@).removeClass "selected")
    element.children().each ->
      self.setupHoverFor $(@)

