class Comment
  constructor: (@comment) ->
    @createElement()

  id: ->
    'comment-' + @comment.id

  createElement: ->
    @element = document.createElement('li')
    @element.id = @id()
    @element.className = 'comment'
    @element.innerHTML = "<div class='author'>Joe User</div><div class='body'>" + @comment.body + "</div>"

  insert: (listElement) ->
    listElement.appendChild(@element)
