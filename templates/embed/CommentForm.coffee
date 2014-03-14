class CommentForm
  constructor: (@comments) ->
    @createElement()
    @bindEvents()

  id: ->
    'comment-form-' + @comments.id()

  createElement: ->
    @element = document.createElement('div')
    @element.id = @id()
    @element.className = 'comment-form'
    @element.innerHTML = "<a>Leave a note</a><form><div class='author'></div><textarea placeholder='Leave a note' class='body'></textarea><input type='submit' value='Save'></form>"

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
    @element.querySelector('a').addEventListener 'click', (event) =>
      event.preventDefault()
      event.stopPropagation()
      if Carnival.isLoggedIn()
        @showCommentForm()
      else
        Carnival.login()
    document.addEventListener 'hasLoggedIn', =>
      @showCommentForm()

  showCommentForm: ->
    @element.querySelector('a').style.display = 'none'
    @element.querySelector('form').style.display = 'block'
    @element.querySelector('.author').innerHTML = Carnival.userName()

  commentHash: ->
    { thread: @comments.thread(), body: @body() }
