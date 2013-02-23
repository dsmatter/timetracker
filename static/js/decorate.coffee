define ->
  decorateOverview: ->
    @setupHoverFor($("body"))
    @setupTasksHover()
    @decorateRunning()

  setupTasksHover: ->
    self = @
    $(".task").each ->
      self.setupTaskHover $(@)

  setupTaskHover: (element) ->
    element.find(".info").hover \
    (->
      element.find(".sessions").removeClass("hidden")),
    (->
      element.find(".sessions").addClass("hidden"))

  setupHoverFor: (element) ->
    self = @
    element.hover \
    (->
      $(@).addClass "selected"),
    (->
      $(@).removeClass "selected")
    element.children().each ->
      self.setupHoverFor $(@)

  decorateRunning: ->
    $(".task .running").each ->
      $(@).closest(".task").addClass "runningTask"

  decorateTask: (element) ->
    @setupHoverFor element
    @setupTaskHover element
    if element.find(".running").length > 0
      element.addClass "runningTask"

