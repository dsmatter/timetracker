define ->
  decorateOverview: ->
    @setupHover()

  setupHover: ->
    $(".task").hover \
    (->
      $(@).addClass "selected"
      $(@).find(".sessions").removeClass("hidden")),
    (->
      $(@).removeClass "selected"
      $(@).find(".sessions").addClass("hidden")
    )
    $(".total").hover \
    (->
      $(@).addClass "selected"),
    (->
      $(@).removeClass "selected"
    )

