define ['components/flight/lib/component', 'decorate', 'task'], (defineComponent, decorator, taskC) ->
  component = ->

    @sumUpSessions = ->
      result = 0
      for selector in [".tasktime:visible", ".running:visible"]
        $(selector).each ->
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

    @replaceTask = (e, task) ->
      element = @$node.find("#task-" + task.taskId)
      decorator.decorateTask element
      taskC.attachTo element, taskId: task.taskId
      @trigger "totalChanged"

    @getSummary = ->
      query = ""
      $(".task:visible").each ->
        id = $(@).attr("id").replace("task-", "")
        query += "-#{id}"
      query = query.substring 1
      window.location = "/summary?tasks=#{query}"

    @after "initialize", ->
      @calculateTotal()
      @on "totalChanged", @calculateTotal
      @on "newTask", @addTask
      @on "replaceTask", @replaceTask
      @on "getSummary", @getSummary

  defineComponent component