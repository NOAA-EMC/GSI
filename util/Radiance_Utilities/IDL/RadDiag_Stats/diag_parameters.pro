  ; ----------------
  ; Numeric literals
  ; ----------------
  POINT25 = 0.25d0
  POINT5  = 0.5d0
  POINT75 = 0.75d0
  ZERO = 0.0d0
  ONE  = 1.0d0
  TWO  = 2.0d0


  ; -------------------
  ; Plotting parameters
  ; -------------------
  ; Plot header position
  XPOS_PLOT_HEADER = 0.05
  YPOS_PLOT_HEADER = 0.98

  ; X/Y-margin defaults
  DEFAULT_XMARGIN = [ 15, 5 ]
  DEFAULT_YMARGIN = [ 4, 2 ]

  ; The default number of plots in the X and Y direction
  DEFAULT_NXPLOTS = 1
  DEFAULT_NYPLOTS = 1

  ; The character size in normalised units
  CHARACTER_SIZE = CONVERT_COORD( !D.X_CH_SIZE, !D.Y_CH_SIZE, /DEVICE, /TO_NORMAL )

  ; Some color and symbol definitions
  DCOLOR = 4
  DSYMBOL = -4

  D2COLOR = 5
  D2SYMBOL = -6

  ECOLOR = 2
