class CommentForm
  constructor: (@comments) ->
    @createElement()
    @bindEvents()

  id: ->
    'comment-form-' + @comments.id()

  createElement: ->
    @element = document.createElement('form')
    @element.id = @id()
    @element.className = 'comment-form'
    @element.innerHTML = "<div class='author'>Joe User</div><textarea placeholder='Leave a note' class='body'></textarea><input type='submit' value='Save'>"

  body: ->
    @element.querySelector('.body').value

  bindEvents: ->
    @element.addEventListener 'submit', (event) =>
      event.preventDefault()
      event.stopPropagation()
      Carnival.post(
        '/comments',
        @commentHash(),
        () =>
          @comments.add(new Comment(@commentHash()))
          @element.querySelector('.body').value = ''
      )

  commentHash: ->
    { thread: @comments.thread(), body: @body() }
