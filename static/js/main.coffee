# DEVELOPMENT cache busting
requirejs.config
  urlArgs: "bust=" + (new Date()).getTime()

requirejs ["decorate", "tasks", "task", "createTask", "total"],
(decorator, tasks, task, createTask, total) ->
  $("document").ready ->
    decorator.decorateOverview()
    createTask.attachTo "#addtask"
    total.attachTo "#totalval"
    $(".task").each ->
      id = $(@).attr("id").replace "task-", ""
      task.attachTo $(@), taskId: id
    tasks.attachTo "#tasks"

