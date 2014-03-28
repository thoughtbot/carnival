class Comments
  constructor: (@block) ->
    @createElement()
    @active = false
    @bindEvents()
    @comments = []
    @fetchComments()
    @renderCommentForm()

  id: ->
    @block.id() + '-comments'

  createElement: =>
    @element = document.createElement('div')
    @element.id = @id()
    @element.setAttribute('data-comments-for', @block.id())
    @element.className = 'comments'
    @element.innerHTML = "<span class='count'></span><ol></ol>"
    @element.style.minHeight = @block.element.offsetHeight + 'px'

  bindEvents: ->
    @element.addEventListener 'mouseenter', =>
      @show()
    @element.addEventListener 'mouseleave', =>
      @hide()
    @countElement().addEventListener 'click', =>
      @toggle()

  activate: =>
    @active = true
    Carnival.addClass(@element, 'commenting')
    @element.dispatchEvent(new CustomEvent("commenting", bubbles: true))

  deactivate: =>
    @active = false
    Carnival.removeClass(@element, 'commenting')
    @element.dispatchEvent(new CustomEvent("doneCommenting", bubbles: true))

  toggle: =>
    if @active
      @deactivate()
    else
      @activate()

  hide: ->
    unless @active or @hasComments()
      @element.style.opacity = '0'

  show: ->
    @element.style.opacity = '1'

  thread: ->
    @block.id()

  fetchComments: ->
    @count(@commentData().length)
    @comments = @commentData().map (comment) ->
      new Comment(comment)
    @insertCommentsIntoDom()
    @showIfComments()

  commentData: ->
    @block.commentData

  insertCommentsIntoDom: ->
    for comment in @comments
      comment.insert(@listElement())

  count: (count) ->
    unless count > 0
      count = '+'
    @countElement().innerHTML = count

  countElement: ->
    @element.querySelector(':scope > .count')

  listElement: ->
    @element.querySelector(':scope > ol')

  renderCommentForm: ->
    @element.appendChild(new CommentForm(this).element)

  showIfComments: ->
    if @hasComments()
      @show()

  hasComments: ->
    @comments.length > 0

  add: (comment) ->
    @comments.push(comment)
    @count(@comments.length)
    @listElement().appendChild(comment.element)
