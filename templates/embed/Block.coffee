class Block
  constructor: (@element, @prefix, @index) ->
    @bindEvents()
    @comments = new Comments(this)
    @element.id = @id()

  id: ->
    @prefix + '-' + @index

  bindEvents: ->
    @element.addEventListener 'mouseenter', =>
      @comments.show()
    @element.addEventListener 'mouseleave', =>
      @comments.hide()

  insert: (articleElement) ->
    articleElement.insertBefore(@comments.element, @element)
