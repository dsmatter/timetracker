define ->
  decorateOverview: ->
    @setupHoverFor($("body"))
    @setupTasksHover()
    @decorateRunning()
    @setupTotalHover()

  setupTasksHover: ->
    self = @
    $(".task").each ->
      self.setupTaskHover $(@)

  setupTaskHover: (element) ->
    element.hover \
    (->
      element.find(".actions").removeClass("unactive")),
    (->
      element.find(".actions").addClass("unactive"))

    element.find(".expand").hover \
    (->
      element.find(".sessions").removeClass("hidden")),
    (->
      unless element.find(".expand").hasClass("active")
        element.find(".sessions").addClass("hidden"))

    element.find(".info").hover \
    (->
      element.find(".expand").addClass("start")),
    (->
      element.find(".expand").removeClass("start")),

  setupTotalHover: ->
    $(".total").hover \
    (->
      $(@).find(".actions").removeClass("unactive")),
    (->
      $(@).find(".actions").addClass("unactive")),

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

