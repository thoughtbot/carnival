class Indicator
  constructor: (@block) ->
    @createElement()
    @active = false
    @bindEvents()
    @count = 0

  id: ->
    @block.id() + '-indicator'

  createElement: =>
    @element = document.createElement('div')
    @element.id = @id()
    @element.setAttribute('data-indicator-for', @block.id())
    @element.className = 'comment-indicator'
    @element.innerHTML = "<span class='count'></span>"
    @element.style.minHeight = @block.element.offsetHeight + 'px'

  bindEvents: ->
    @element.addEventListener 'mouseenter', =>
      @show()
    @element.addEventListener 'mouseleave', =>
      @hide()
    @countElement().addEventListener 'click', =>
      @toggle()
    document.addEventListener 'commenting', (event) =>
      if @active and event.detail != @block
        @deactivate()
        @hide()

  activate: =>
    @active = true
    Carnival.addClass(@element, 'commenting')
    @block.activate()

  deactivate: =>
    @active = false
    Carnival.removeClass(@element, 'commenting')
    @block.deactivate()

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

  setCount: (@count) ->
    @countElement().innerHTML = @displayCount()
    @showIfComments()

  displayCount: ->
    if @count > 0 then @count else '+'

  countElement: ->
    @element.querySelector(':scope > .count')

  showIfComments: ->
    if @hasComments()
      @show()

  hasComments: ->
    @count > 0
