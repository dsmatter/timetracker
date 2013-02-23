define ->

  request: (method, path, data, callback) ->
    $.ajax path,
      type: method
      data: data
      error: (xhr, status, error) -> callback (new Error status + " | " + error)
      success: (data, status, xhr) -> callback null, data, status, xhr

  get: (path, data, callback) ->
    @request "GET", path, data, callback

  post: (path, data, callback) ->
    @request "POST", path, data, callback

  delete: (path, data, callback) ->
    @request "DELETE", path, data, callback

  put: (path, data, callback) ->
    @request "PUT", path, data, callback
