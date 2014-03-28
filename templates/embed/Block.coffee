class Block
  constructor: (@element, @articleId, @index) ->
    @bindEvents()
    @element.id = @id()

  id: ->
    @articleId + '-' + @index

  bindEvents: ->
    @element.addEventListener 'mouseenter', =>
      @comments.show()
    @element.addEventListener 'mouseleave', =>
      @comments.hide()

  insert: (articleElement) ->
    @comments = new Comments(this)
    articleElement.insertBefore(@comments.element, @element)
