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
    @element.className = 'carnival-comment-indicator'
    @element.innerHTML = "<span class='carnival-count'></span>" + @iconHTML()

  iconHTML: ->
    '<svg><path d="M0.848275862,0 C0.38,0 0,0.508615385 0,1.13538462 L0,18.4006154 C0,19.0273846 0.38,19.536 0.848275862,19.536 L4.16896552,19.536 L4.16896552,22.8 C4.16896552,23.6815385 4.60068966,23.9704615 5.12758621,23.4433846 L9.03448276,19.5369231 L19.2206897,19.5369231 C19.6889655,19.5369231 20.0689655,19.0283077 20.0689655,18.4015385 L20.0689655,1.13538462 C20.0696552,0.508615385 19.6896552,3.68935651e-15 19.2213793,3.68935651e-15 L0.848275862,0 Z"></path></svg>'

  bindEvents: ->
    @element.addEventListener 'mouseenter', =>
      @show()
    @element.addEventListener 'mouseleave', =>
      @hide()
    @element.addEventListener 'click', =>
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
    @element.querySelector(':scope > .carnival-count')

  showIfComments: ->
    if @hasComments()
      @show()

  hasComments: ->
    @count > 0
