define ['components/flight/lib/component', 'ajax', 'time', 'session'],
(defineComponent, ajax, time, session) ->
  component = ->

    @delete = ->
      ajax.delete "/tasks/" + @attr.taskId, "", (err, data) =>
        if err?
          alert err
          return
        @$node.fadeOut =>
          @trigger "#tasks", "totalChanged"
          @$node.remove()

    @nameWithTags = ->
      result = @$node.find(".taskname").text()
      tags = @$node.find(".tags").text()
      if tags != ""
        return result + " " + tags
      else
        return result

    @enterEditMode = ->
      nameElement = @$node.find ".taskname"
      oldName = @nameWithTags()
      nameElement.html('<input type="text" value="' + oldName + '" />')
      inputElement = nameElement.find "input"
      inputElement.focus()
      inputElement.keydown (e) =>
        if e.which == 13
          @exitEditMode inputElement

    @exitEditMode = (input) ->
      name = input.val()
      ajax.put "/tasks/" + @attr.taskId, name: name, (err, data) =>
        if err?
          alert err
          return
        newName = $(data).find ".taskname"
        newTags = $(data).find ".tags"
        @$node.find(".taskname").replaceWith newName
        @$node.find(".tags").replaceWith newTags

    @getRunningDate = ->
      secs = @$node.find(".started").text()
      new Date (Number) secs

    @updateRunningDate = ->
      @$node.find(".running").text (time.dateDiffSince @getRunningDate())

    @setupStarted = ->
      @updateRunningDate()
      @attr.timer = setInterval (=> 
        @updateRunningDate()
        @trigger "#tasks", "totalChanged"
      ), 10000

    @taskCmd = (cmd) ->
      ajax.post "/tasks/" + @attr.taskId + "/" + cmd, "", (err, data) =>
        if err?
          alert err
          return
        @trigger (@$node.find ".times tr"), "sessionTearDown"
        @trigger "replaceTask",
          taskId: @attr.taskId
          element: $(data)

    @start = ->
      @taskCmd "start"

    @stop = ->
      @taskCmd "stop"

    @toggleExpand = ->
      expand = @$node.find ".expand"
      sessions = @$node.find ".sessions"
      if expand.hasClass "active"
        sessions.addClass "hidden"
      else
        sessions.removeClass "hidden"
      @$node.find(".expand").toggleClass("active").show()

    @after "initialize", ->
      self = @
      @$node.find(".delete").click =>
        if confirm "Delete task '" + @$node.find(".taskname").text() + "'?"
          @trigger "delete"

      @$node.find(".edit").click =>
        @trigger "edit"

      @$node.find(".info").click =>
        if @$node.find(".running").length > 0
          @trigger "stop"
        else
          @trigger "start"

      @$node.find(".expand").click =>
        @trigger "toggleExpand"

      @$node.find(".times tr").each ->
        id = $(@).attr("id").replace "session-", ""
        session.attachTo $(@),
          sessionId: id
          taskId: self.attr.taskId

      if @$node.find(".started").length > 0
        @setupStarted()

      @on "delete", @delete
      @on "edit", @enterEditMode
      @on "start", @start
      @on "stop", @stop
      @on "toggleExpand", @toggleExpand
      @on "replaceTask", -> @teardown()

  defineComponent component