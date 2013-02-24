define ['components/flight/lib/component', 'ajax', 'time'], (defineComponent, ajax, time) ->
  component = ->

    @delete = ->
      ajax.delete "/sessions/#{@attr.sessionId}", "", (err, data) =>
        if err?
          alert err
          return
        @teardown()
        @trigger "replaceTask",
          taskId: @attr.taskId
          element: $(data)

    @after "initialize", ->
      @$node.click =>
        sessionText = ""
        @$node.find("td").each -> sessionText += " " + $(@).text()
        sessionText = sessionText.substring 1
        if confirm "Really delete session '" + sessionText + "'?"
          @delete()

      @on "sessionTearDown", ->
        @teardown()

  defineComponent component