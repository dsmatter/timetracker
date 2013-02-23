define ['components/flight/lib/component', 'ajax', 'time'], (defineComponent, ajax, time) ->
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
        newElement = $(data).find ".task"
        @$node.replaceWith newElement
        @teardown()
        @trigger "#tasks", "replaceTask", taskId: @attr.taskId

    @start = ->
      @taskCmd "start"

    @stop = ->
      @taskCmd "stop"

    @after "initialize", ->
      @$node.find(".delete").click =>
        if confirm "Delete task '" + @$node.find(".taskname").text() + "'?"
          @trigger "delete"

      @$node.find(".edit").click =>
        @trigger "edit"

      @$node.find(".info").click =>
        if @$nod  e.find(".running").length > 0
          @trigger "stop"
        else
          @trigger "start"

      if @$node.find(".started").length > 0
        @setupStarted()

      @on "delete", @delete
      @on "edit", @enterEditMode
      @on "start", @start
      @on "stop", @stop

  defineComponent component