define ['components/flight/lib/component', 'ajax', 'time'], (defineComponent, ajax, time) ->
  component = ->

    @initEditSession = ->
      element = $(".sessionEdit")
      pos = @$node.offset()
      date = @$node.closest(".date").text().match(/\d{4}-\d{2}-\d{2}/)[0]
      startTime = @$node.find(".starttime").text()
      endTime = @$node.find(".endtime").text()
      element.css "top", pos.top + "px"
      element.css "left", pos.left + "px"
      element.css "padding", "20px"
      element.find("#startTime").val date + startTime
      element.find("#endTime").val date + endTime
      element.unbind "dblclick"
      element.dblclick ->
        $(@).hide()
      updateButton = element.find "#editSubmit"
      updateButton.unbind "click"
      updateButton.click =>
        startTime = element.find("#startTime").val()
        endTime = element.find("#endTime").val()
        @update startTime, endTime
        element.hide()
      deleteButton = element.find "#editDelete"
      deleteButton.unbind "click"
      deleteButton.click =>
        sessionText = ""
        @$node.find("td").each -> sessionText += " " + $(@).text()
        sessionText = sessionText.substring 1
        if confirm "Really delete session '" + sessionText + "'?"
          @delete()
          element.hide()
      element.show()

    @delete = ->
      ajax.delete "/sessions/#{@attr.sessionId}", "", (err, data) =>
        if err?
          alert err
          return
        @teardown()
        @trigger "replaceTask",
          taskId: @attr.taskId
          element: $(data)

    @update = (start, end) ->
      ajax.post "/sessions/#{@attr.sessionId}", "start=#{start}&end=#{end}", (err, data) =>
        if err?
          alert err
          return
        @trigger "replaceTask",
          taskId: @attr.taskId
          element: $(data)

    @after "initialize", ->
      @$node.click =>
        @initEditSession()

      @on "sessionTearDown", ->
        @teardown()

  defineComponent component
