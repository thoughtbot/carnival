class Comment
  constructor: (@comment) ->
    @createElement()

  id: ->
    'comment-' + @comment.id

  createElement: ->
    @element = document.createElement('li')
    @element.id = @id()
    @element.className = 'comment'
    @element.innerHTML = "<div class='author'><img src='#{@comment.gravatar_url}'><span>#{@comment.user_name}</span></div><div class='body'>#{@comment.body_html}</div>"

  insert: (listElement) ->
    listElement.appendChild(@element)
