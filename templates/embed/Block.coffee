class Block
  constructor: (@element, @articleId, @index) ->
    @bindEvents()
    @element.id = @id()
    @comments = []
    @indicator = new Indicator(this)

  id: ->
    @articleId + '-' + @index

  bindEvents: ->
    @element.addEventListener 'mouseenter', =>
      @indicator.show()
    @element.addEventListener 'mouseleave', =>
      @indicator.hide()

  insert: (articleElement) ->
    articleElement.insertBefore(@indicator.element, @element)

  setComments: (commentData) ->
    @comments = commentData.map (comment) ->
      new Comment(comment)
    @indicator.setCount(@comments.length)

  activate: =>
    Carnival.addClass(@element, 'commenting')
    @element.dispatchEvent(new CustomEvent("commenting", bubbles: true, detail: this))

  deactivate: =>
    Carnival.removeClass(@element, 'commenting')
    @element.dispatchEvent(new CustomEvent("doneCommenting", bubbles: true, detail: this))
