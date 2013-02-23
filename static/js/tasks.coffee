define ['components/flight/lib/component', 'decorate', 'task'], (defineComponent, decorator, taskC) ->
  component = ->

    @sumUpSessions = ->
      result = 0
      $(".tasktime:visible").each ->
        val = $(@).text().trim()
        split = val.split ":"
        result += 60 * ((Number) split[0]) + ((Number) split[1])
      result

    @addTask = (e, task) ->
      @$node.append task.html
      taskId = $(task.html).find(".task").attr("id").replace "task-", ""
      taskElement = $("#task-" + taskId)
      decorator.setupHoverFor taskElement
      taskC.attachTo taskElement, taskId: taskId

    @calculateTotal = ->
      @trigger $("#totalval"), "setTotal", secs: @sumUpSessions()

    @after "initialize", ->
      @calculateTotal()
      @on "totalChanged", @calculateTotal
      @on "newTask", @addTask

  defineComponent component