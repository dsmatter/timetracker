define ->

  request: (method, path, data, callback) ->
    timer = setTimeout (->
      $(".overlay").show()
    ),  100
    $.ajax path,
      type: method
      data: data
      error: (xhr, status, error) ->
        clearTimeout timer
        $(".overlay").hide()
        callback (new Error status + " | " + error)
      success: (data, status, xhr) ->
        clearTimeout timer
        $(".overlay").hide()
        callback null, data, status, xhr

  get: (path, data, callback) ->
    @request "GET", path, data, callback

  post: (path, data, callback) ->
    @request "POST", path, data, callback

  delete: (path, data, callback) ->
    @request "DELETE", path, data, callback

  put: (path, data, callback) ->
    @request "PUT", path, data, callback
