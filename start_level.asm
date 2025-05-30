xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;===============================================================================
; CHAR PAD TOOLS
;===============================================================================
; Peter 'Sig' Hewett 2017
;-------------------------------------------------------------------------------
; Tools for integrating CharPad character sets, tiles, and maps
;===============================================================================
; Map Notes:
; 
; CharPad setup : To make a level map, set up for 256 characters
;                 A tile size of 4x4
;                 Set number of tiles to 64
;                 Set map size to 64 x 32
;                 *IMPORTANT* set color to 'per character'
;

;-------------------------------------------------------------------------------
;                                                               DRAW MAP 
;-------------------------------------------------------------------------------
; Draw the entire map on the screen. This won't be done that often as most updates
; to the screen will be scrolling. But for starting a level, resetting on death,
; or teleporting, we need to build the entire screen.
; This also sets up essential data for using the map.
; Initializes : MAP_POS_X
;               MAP_POS_Y
;               MAP_POS_ADDRESS
;               MAP_X_DELTA
;               MAP_Y_DELTA
;
; X = Start map X coord (top left corner)
; Y = Start map Y coord (top left corner)
;
; Uses ZEROPAGE_POINTER_4 (as TileDraw uses 1,2,3)
;-------------------------------------------------------------------------------
#region "DrawMap"
DrawMap

        lda #0
        sta MAP_X_DELTA
        sta MAP_Y_DELTA

        stx MAP_X_POS
        sty MAP_Y_POS

        ;---------------------------------------------------------------
        ; First find the address for the starting map position

        ldx MAP_Y_POS

        lda MAP2025_LINE_LOOKUP_LO,x        ; fetch the address for the line (Y pos)
        sta ZEROPAGE_POINTER_4
        lda MAP2025_LINE_LOOKUP_HI,x
        sta ZEROPAGE_POINTER_4 + 1

        clc
        lda ZEROPAGE_POINTER_4          ; add the x position
        adc MAP_X_POS
        sta ZEROPAGE_POINTER_4
        lda ZEROPAGE_POINTER_4 + 1
        adc #0
        sta ZEROPAGE_POINTER_4 + 1      ; ZEROPAGE_POINTER_1 now holds the 
                                        ; map start address

                                        ; Save this info for map usage
        copyPointer ZEROPAGE_POINTER_4, MAP_POS_ADDRESS
        ;-----------------------------------------------------------------------
        ; Fetch map data and draw tile - coords are in 'tiles' not 
        ; character positions

        ldy #0                  ; holds X screen coord
        ldx #0                  ; holds Y screen coord

@loop
        lda (ZEROPAGE_POINTER_4),y              ; fetch map data
        jsr DrawTile                            ; draw the tile

        iny                                     ; inc X and check for end of screen
        cpy #10                                 ; (10 tiles)
        bne @loop
                                                ; go down one line on the map (64 char)
        addPointer ZEROPAGE_POINTER_4, 16
        ldy #0
        inx
        cpx #6
        bne @loop
        rts
#endregion

;-------------------------------------------------------------------------------
;                                                               DRAW TILE 
;-------------------------------------------------------------------------------
; This routine actually won't be called that often. Most updates are scrolling,
; so there are very few circumstances that need a whole tile drawn at once.
;-------------------------------------------------------------------------------
;
; X = Screen tile Y coord    - 'flipped' so it dovetails into the MapDraw
; Y = Screen tile X coord       routine without exhanging data in registers   
; A = Tile # to draw
;
; Restores registers off the stack A / X / Y
;-------------------------------------------------------------------------------

#region "DrawTile"
DrawTile
        sta PARAM1                      ; save tile number
        sty PARAM2                      ; save X pos
        stx PARAM3                      ; save Y pos

        saveRegs                        ; put registers on the stack to
                                        ; exit cleaner - this routine will
                                        ; likely be nested
        ;------------------------------------------------------
        ; First get the destination for the tile

        lda PARAM3                      ; fetch the Y pos (in tile coords)
        asl
        asl                             ; multiply by 4 (tiles are 4 x 4 chars)
        tax                             ; screen line in X

        jsr GetScreenLineAddress      ; fetch line address based on current displayed screen
                                        ; Y line address is in ZEROPAGE_POINTER_1


        lda COLOR_LINE_OFFSET_TABLE_LO,x        ; fetch color ram line address too
        sta ZEROPAGE_POINTER_3
        lda COLOR_LINE_OFFSET_TABLE_HI,x
        sta ZEROPAGE_POINTER_3 + 1
        
        lda PARAM2                      ; get X coord
        asl                             ; multiply by 4
        asl
        tax                             ; save it in x
        clc                             ; add to Y line address
        adc ZEROPAGE_POINTER_1
        sta ZEROPAGE_POINTER_1
        lda ZEROPAGE_POINTER_1 + 1
        adc #0
        sta ZEROPAGE_POINTER_1 + 1      ; destination base address is in ZEROPAGE_POINTER_1
        txa
        clc
        adc ZEROPAGE_POINTER_3          ; color ram destination is in ZEROPAGE_POINTER_3
        sta ZEROPAGE_POINTER_3
        lda ZEROPAGE_POINTER_3 + 1
        adc #0
        sta ZEROPAGE_POINTER_3 + 1

        ;------------------------------------------------------------
        ; Fetch the source tile address

        ldx PARAM1                      ; Fetch the tile number       
        lda TILE_NUMBER_LOOKUP_LO,x
        sta ZEROPAGE_POINTER_2
        lda TILE_NUMBER_LOOKUP_HI,x
        sta ZEROPAGE_POINTER_2 + 1
        ;-------------------------------------------------------------
        ; Loop through and draw the tile 

        ldy #0
@drawloop
        lda (ZEROPAGE_POINTER_2),y      ; Get the character code
        sta (ZEROPAGE_POINTER_1),y      ; store it on the screen
        tax                             ; pass to X as an offset
        lda ATTRIBUTE_MEM,x             ; fetch the color/data attribute
        sta (ZEROPAGE_POINTER_3),y      ; write it to color ram
        
        cpy #15                         ; drawn the 15th character? We're finished
        beq @done

        tya                             ; save Y before the increment for our test
        iny                             ; (saves having to inc it in 2 diff places)

                                        ; I need to count 0-3 to draw a row of tiles,                    
        and #%00000011                  ; but I need to count 0-15 to fetch the tile data
        cmp #3                          ; both NEED to use indirect Y addressing, and saving/
        bne @drawloop                   ; fetching Y rapidly becomes a tangled nightmare.
                                        ; by masking out the last 2 bits in A, we get a number
                                        ; that counts 0-3 over and over without stopping the
                                        ; data fetch count 0-15. It's also faster than my other
                                        ; options by quite a bit.


        clc                             ; add new line
        lda ZEROPAGE_POINTER_1          ; increment destination and color ram by 1 line - 4 chars
        adc #40 - 4
        sta ZEROPAGE_POINTER_1          ; by 'backsetting' our pointers, we don't need to change Y
        lda ZEROPAGE_POINTER_1 + 1      ; when drawing 0-3 characters, and can leave the 0-15
        adc #0                          ; count intact.
        sta ZEROPAGE_POINTER_1 + 1      ; We have to increase them to the next line anyways, so this
                                        ; only saves time.
        clc
        lda ZEROPAGE_POINTER_3
        adc #40 - 4
        sta ZEROPAGE_POINTER_3
        lda ZEROPAGE_POINTER_3 + 1
        adc #0
        sta ZEROPAGE_POINTER_3 + 1        
        jmp @drawloop
@done
        restoreRegs             ; pull registers back off the stack        
        rts

#endRegion




#region "DrawMap"
TileMap

        lda #0
        sta MAP_X_DELTA
        sta MAP_Y_DELTA

        stx MAP_X_POS
        sty MAP_Y_POS

        ;---------------------------------------------------------------
        ; First find the address for the starting map position

        ldx MAP_Y_POS

        lda ALLEYMAP_LINE_LOOKUP_LO,x
        sta ZEROPAGE_POINTER_4
        lda ALLEYMAP_LINE_LOOKUP_HI,x
        sta ZEROPAGE_POINTER_4 + 1

        clc
        lda ZEROPAGE_POINTER_4          ; add the x position
        adc MAP_X_POS
        sta ZEROPAGE_POINTER_4
        lda ZEROPAGE_POINTER_4 + 1
        adc #0
        sta ZEROPAGE_POINTER_4 + 1      ; ZEROPAGE_POINTER_1 now holds the map 
                                        ; start address

                                        ; Save this info for map usage
        copyPointer ZEROPAGE_POINTER_4, MAP_POS_ADDRESS
        ;-----------------------------------------------------------------------
        ; Fetch map data and draw tile - coords are in 'tiles' not 
        ; character positions

        ldy #0                  ; holds X screen coord
        ldx #0                  ; holds Y screen coord

@loop
        lda (ZEROPAGE_POINTER_4),y              ; fetch map data
        jsr TileDraw                            ; draw the tile

        iny                                     ; inc X and check for end of screen
        cpy #10                                 ; (10 tiles)
        bne @loop
                                                ; go down one line on the map (64 char)
        addPointer ZEROPAGE_POINTER_4, 17
        ldy #0
        inx
        cpx #6
        bne @loop
        rts
#endregion

;-------------------------------------------------------------------------------
;                                                               DRAW TILE 
;-------------------------------------------------------------------------------
; This routine actually won't be called that often. Most updates are scrolling,
; so there are very few circumstances that need a whole tile drawn at once.
;-------------------------------------------------------------------------------
;
; X = Screen tile Y coord    - 'flipped' so it dovetails into the MapDraw
; Y = Screen tile X coord       routine without exhanging data in registers   
; A = Tile # to draw
;
; Restores registers off the stack A / X / Y
;-------------------------------------------------------------------------------

#region "DrawTile"
TileDraw
        sta PARAM1                      ; save tile number
        sty PARAM2                      ; save X pos
        stx PARAM3                      ; save Y pos

        saveRegs                        ; put registers on the stack to
                                        ; exit cleaner - this routine will
                                        ; likely be nested
        ;------------------------------------------------------
        ; First get the destination for the tile

        lda PARAM3                      ; fetch the Y pos (in tile coords)
        asl
        asl                             ; multiply by 4 (tiles are 4 x 4 chars)
        tax                             ; screen line in X

        jsr GetScreenLineAddress      ; fetch line address based on current displayed screen
                                        ; Y line address is in ZEROPAGE_POINTER_1


        lda COLOR_LINE_OFFSET_TABLE_LO,x        ; fetch color ram line address too
        sta ZEROPAGE_POINTER_3
        lda COLOR_LINE_OFFSET_TABLE_HI,x
        sta ZEROPAGE_POINTER_3 + 1
        
        lda PARAM2                      ; get X coord
        asl                             ; multiply by 4
        asl
        tax                             ; save it in x
        clc                             ; add to Y line address
        adc ZEROPAGE_POINTER_1
        sta ZEROPAGE_POINTER_1
        lda ZEROPAGE_POINTER_1 + 1
        adc #0
        sta ZEROPAGE_POINTER_1 + 1      ; destination base address is in ZEROPAGE_POINTER_1
        txa
        clc
        adc ZEROPAGE_POINTER_3          ; color ram destination is in ZEROPAGE_POINTER_3
        sta ZEROPAGE_POINTER_3
        lda ZEROPAGE_POINTER_3 + 1
        adc #0
        sta ZEROPAGE_POINTER_3 + 1

        ;------------------------------------------------------------
        ; Fetch the source tile address

        ldx PARAM1                      ; Fetch the tile number       
        lda TILE2_NUMBER_LOOKUP_LO,x
        sta ZEROPAGE_POINTER_2
        lda TILE2_NUMBER_LOOKUP_HI,x
        sta ZEROPAGE_POINTER_2 + 1
        ;-------------------------------------------------------------
        ; Loop through and draw the tile 

        ldy #0
@drawloop
        lda (ZEROPAGE_POINTER_2),y      ; Get the character code
        sta (ZEROPAGE_POINTER_1),y      ; store it on the screen
        tax                             ; pass to X as an offset
        lda ATTRIBUTE2_MEM,x             ; fetch the color/data attribute
        sta (ZEROPAGE_POINTER_3),y      ; write it to color ram
        
        cpy #15                         ; drawn the 15th character? We're finished
        beq @done

        tya                             ; save Y before the increment for our test
        iny                             ; (saves having to inc it in 2 diff places)

                                        ; I need to count 0-3 to draw a row of tiles,                    
        and #%00000011                  ; but I need to count 0-15 to fetch the tile data
        cmp #3                          ; both NEED to use indirect Y addressing, and saving/
        bne @drawloop                   ; fetching Y rapidly becomes a tangled nightmare.
                                        ; by masking out the last 2 bits in A, we get a number
                                        ; that counts 0-3 over and over without stopping the
                                        ; data fetch count 0-15. It's also faster than my other
                                        ; options by quite a bit.


        clc                             ; add new line
        lda ZEROPAGE_POINTER_1          ; increment destination and color ram by 1 line - 4 chars
        adc #40 - 4
        sta ZEROPAGE_POINTER_1          ; by 'backsetting' our pointers, we don't need to change Y
        lda ZEROPAGE_POINTER_1 + 1      ; when drawing 0-3 characters, and can leave the 0-15
        adc #0                          ; count intact.
        sta ZEROPAGE_POINTER_1 + 1      ; We have to increase them to the next line anyways, so this
                                        ; only saves time.
        clc
        lda ZEROPAGE_POINTER_3
        adc #40 - 4
        sta ZEROPAGE_POINTER_3
        lda ZEROPAGE_POINTER_3 + 1
        adc #0
        sta ZEROPAGE_POINTER_3 + 1        
        jmp @drawloop
@done
        restoreRegs             ; pull registers back off the stack        
        rts

;===============================================================================
; LEVEL DATA AND TABLES
;===============================================================================

CURRENT_LEVEL
        byte 0

;CHAR_ADDRESS
;        word LEVEL_1_CHARS

ATTRIB_ADDRESS
        word ATTRIBUTE_MEM

TILE_ADDRESS
        word TILE_MEM

MAP_ADDRESS
        word LEVEL_1_MAP
;-------------------------------------------------------------------------------
; MAP DATA LOOKUP TABLE 1
;-------------------------------------------------------------------------------
; Lookup table to return an address to a map line (Y coord). This table assumes 
; a landscape
; layout (64 x 32 tiles). A portrait style table will be done in the future to 
; allow more vertical maps
;-------------------------------------------------------------------------------
MAP_LINE_LOOKUP_LO
;        byte <MAP_MEM
;        byte <MAP_MEM + 100
;        byte <MAP_MEM + 200
;        byte <MAP_MEM + 300
;        byte <MAP_MEM + 400
;        byte <MAP_MEM + 500
;        byte <MAP_MEM + 600
;        byte <MAP_MEM + 700
;        byte <MAP_MEM + 800
;        byte <MAP_MEM + 900
;        byte <MAP_MEM + 1000             ; 10
;        byte <MAP_MEM + 1100
;        byte <MAP_MEM + 1200
;        byte <MAP_MEM + 1300
;        byte <MAP_MEM + 1400
;        byte <MAP_MEM + 1500
;        byte <MAP_MEM + 1600
;        byte <MAP_MEM + 1700
;        byte <MAP_MEM + 1800
;        byte <MAP_MEM + 1900
;        byte <MAP_MEM + 2000            ;20
;        byte <MAP_MEM + 2100
;        byte <MAP_MEM + 2200
;        byte <MAP_MEM + 2300
;        byte <MAP_MEM + 2400
;        byte <MAP_MEM + 2500
;        byte <MAP_MEM + 2600
;        byte <MAP_MEM + 2700
;        byte <MAP_MEM + 2800
;        byte <MAP_MEM + 2900
;        byte <MAP_MEM + 3000            ;30
;        byte <MAP_MEM + 3100
;        byte <MAP_MEM + 3200            ;32

;;; New lines - Parkour Big Map
;        byte <MAP_MEM + 3300
;        byte <MAP_MEM + 3400
;        byte <MAP_MEM + 3500
;        byte <MAP_MEM + 3600
;        byte <MAP_MEM + 3700
;        byte <MAP_MEM + 3800
;        byte <MAP_MEM + 3900
;        byte <MAP_MEM + 4000
;        byte <MAP_MEM + 4100
;        byte <MAP_MEM + 4200
;        byte <MAP_MEM + 4300
;        byte <MAP_MEM + 4400
;        byte <MAP_MEM + 4500

;        byte <MAP_MEM + 4600
;        byte <MAP_MEM + 4700
;        byte <MAP_MEM + 4800
;        byte <MAP_MEM + 4900
;        byte <MAP_MEM + 5000

;        byte <MAP_MEM + 5010
;        byte <MAP_MEM + 5020
;        byte <MAP_MEM + 5030
;        byte <MAP_MEM + 5040
;        byte <MAP_MEM + 5050

MAP_LINE_LOOKUP_HI
;        byte >MAP_MEM
;        byte >MAP_MEM + 100
;        byte >MAP_MEM + 200
;        byte >MAP_MEM + 300
;        byte >MAP_MEM + 400
;        byte >MAP_MEM + 500
;        byte >MAP_MEM + 600
;        byte >MAP_MEM + 700
;        byte >MAP_MEM + 800
;        byte >MAP_MEM + 900
;        byte >MAP_MEM + 1000            ; 10
;        byte >MAP_MEM + 1100
;        byte >MAP_MEM + 1200
;        byte >MAP_MEM + 1300
;        byte >MAP_MEM + 1400
;        byte >MAP_MEM + 1500
;        byte >MAP_MEM + 1600
;        byte >MAP_MEM + 1700
;        byte >MAP_MEM + 1800
;        byte >MAP_MEM + 1900
;        byte >MAP_MEM + 2000            ;20
;        byte >MAP_MEM + 2100
;        byte >MAP_MEM + 2200
;        byte >MAP_MEM + 2300        ; byte >MAP_MEM + 4700
;        byte >MAP_MEM + 2400
;        byte >MAP_MEM + 2500
;        byte >MAP_MEM + 2600
;        byte >MAP_MEM + 2700
;        byte >MAP_MEM + 2800
;        byte >MAP_MEM + 2900            ;30
;        byte >MAP_MEM + 3000
;        byte >MAP_MEM + 3100            ;32

MAP2025_LINE_LOOKUP_LO
        byte <MAP_MEM
        byte <MAP_MEM + TILE_DEPTH1; 22
        byte <MAP_MEM + TILE_DEPTH2 ;44
        byte <MAP_MEM + TILE_DEPTH3 ;66
        byte <MAP_MEM + TILE_DEPTH4 ;88
        byte <MAP_MEM + TILE_DEPTH5 ;110
        byte <MAP_MEM + TILE_DEPTH6 ;132
        byte <MAP_MEM + TILE_DEPTH7 ;154
        byte <MAP_MEM + TILE_DEPTH8 ;176
        byte <MAP_MEM + TILE_DEPTH9 ;198
        byte <MAP_MEM + TILE_DEPTH10 ;220
        byte <MAP_MEM + TILE_DEPTH11 ;242             ; 10
        byte <MAP_MEM + TILE_DEPTH12 ;'264
        byte <MAP_MEM + TILE_DEPTH13 ;286
        byte <MAP_MEM + TILE_DEPTH14 ;308
        byte <MAP_MEM + TILE_DEPTH15 ;330
        byte <MAP_MEM + TILE_DEPTH16 ;352
        byte <MAP_MEM + TILE_DEPTH17 ;374
        byte <MAP_MEM + TILE_DEPTH18 ;396
        byte <MAP_MEM + TILE_DEPTH19 ;418
        byte <MAP_MEM + TILE_DEPTH20 ;440
        byte <MAP_MEM + TILE_DEPTH21 ;462            ;20


MAP2025_LINE_LOOKUP_HI
        byte >MAP_MEM
        byte >MAP_MEM + TILE_DEPTH1; 22
        byte >MAP_MEM + TILE_DEPTH2 ;44
        byte >MAP_MEM + TILE_DEPTH3 ;66
        byte >MAP_MEM + TILE_DEPTH4 ;88
        byte >MAP_MEM + TILE_DEPTH5 ;110
        byte >MAP_MEM + TILE_DEPTH6 ;132
        byte >MAP_MEM + TILE_DEPTH7 ;154
        byte >MAP_MEM + TILE_DEPTH8 ;176
        byte >MAP_MEM + TILE_DEPTH9 ;198
        byte >MAP_MEM + TILE_DEPTH10 ;220
        byte >MAP_MEM + TILE_DEPTH11 ;242             ; 10
        byte >MAP_MEM + TILE_DEPTH12 ;'264
        byte >MAP_MEM + TILE_DEPTH13 ;286
        byte >MAP_MEM + TILE_DEPTH14 ;308
        byte >MAP_MEM + TILE_DEPTH15 ;330
        byte >MAP_MEM + TILE_DEPTH16 ;352
        byte >MAP_MEM + TILE_DEPTH17 ;374
        byte >MAP_MEM + TILE_DEPTH18 ;396
        byte >MAP_MEM + TILE_DEPTH19 ;418
        byte >MAP_MEM + TILE_DEPTH20 ;440
        byte >MAP_MEM + TILE_DEPTH21 ;462            ;20

ALLEYMAP_LINE_LOOKUP_LO
        byte <MAP2_MEM
        byte <MAP2_MEM + TILE2_DEPTH1; 22
        byte <MAP2_MEM + TILE2_DEPTH2 ;44
        byte <MAP2_MEM + TILE2_DEPTH3 ;66
        byte <MAP2_MEM + TILE2_DEPTH4 ;88
        byte <MAP2_MEM + TILE2_DEPTH5 ;110
        byte <MAP2_MEM + TILE2_DEPTH6 ;132
        byte <MAP2_MEM + TILE2_DEPTH7 ;154
        byte <MAP2_MEM + TILE2_DEPTH8 ;176
        byte <MAP2_MEM + TILE2_DEPTH9 ;198
        byte <MAP2_MEM + TILE2_DEPTH10 ;220
        byte <MAP2_MEM + TILE2_DEPTH11 ;242             ; 10
        byte <MAP2_MEM + TILE2_DEPTH12 ;'264
        byte <MAP2_MEM + TILE2_DEPTH13 ;286
        byte <MAP2_MEM + TILE2_DEPTH14 ;308
        byte <MAP2_MEM + TILE2_DEPTH15 ;330
        byte <MAP2_MEM + TILE2_DEPTH16 ;352
        byte <MAP2_MEM + TILE2_DEPTH17 ;374
        byte <MAP2_MEM + TILE2_DEPTH18 ;396
        byte <MAP2_MEM + TILE2_DEPTH19 ;418
        byte <MAP2_MEM + TILE2_DEPTH20 ;440
        byte <MAP2_MEM + TILE2_DEPTH21 ;462            ;20


ALLEYMAP_LINE_LOOKUP_HI
        byte >MAP2_MEM
        byte >MAP2_MEM + TILE2_DEPTH1; 22
        byte >MAP2_MEM + TILE2_DEPTH2 ;44
        byte >MAP2_MEM + TILE2_DEPTH3 ;66
        byte >MAP2_MEM + TILE2_DEPTH4 ;88
        byte >MAP2_MEM + TILE2_DEPTH5 ;110
        byte >MAP2_MEM + TILE2_DEPTH6 ;132
        byte >MAP2_MEM + TILE2_DEPTH7 ;154
        byte >MAP2_MEM + TILE2_DEPTH8 ;176
        byte >MAP2_MEM + TILE2_DEPTH9 ;198
        byte >MAP2_MEM + TILE2_DEPTH10 ;220
        byte >MAP2_MEM + TILE2_DEPTH11 ;242             ; 10
        byte >MAP2_MEM + TILE2_DEPTH12 ;'264
        byte >MAP2_MEM + TILE2_DEPTH13 ;286
        byte >MAP2_MEM + TILE2_DEPTH14 ;308
        byte >MAP2_MEM + TILE2_DEPTH15 ;330
        byte >MAP2_MEM + TILE2_DEPTH16 ;352
        byte >MAP2_MEM + TILE2_DEPTH17 ;374
        byte >MAP2_MEM + TILE2_DEPTH18 ;396
        byte >MAP2_MEM + TILE2_DEPTH19 ;418
        byte >MAP2_MEM + TILE2_DEPTH20 ;440
        byte >MAP2_MEM + TILE2_DEPTH21 ;462            ;20

;-------------------------------------------------------------------------------
; TILE ADDRESS LOOKUP TABLE
;-------------------------------------------------------------------------------
; Lookup table to find the start address of a tile on the current map. 
; All current level tiles are
; held in TILE_MEM, there are 64 entries
;-------------------------------------------------------------------------------

TILE_NUMBER_LOOKUP_LO
        byte <TILE_MEM                  ; 0
        byte <TILE_MEM + 16
        byte <TILE_MEM + 32
        byte <TILE_MEM + 48
        byte <TILE_MEM + 64
        byte <TILE_MEM + 80
        byte <TILE_MEM + 96
        byte <TILE_MEM + 112
        byte <TILE_MEM + 128
        byte <TILE_MEM + 144
        byte <TILE_MEM + 160            ; 10
        byte <TILE_MEM + 176
        byte <TILE_MEM + 192
        byte <TILE_MEM + 208
        byte <TILE_MEM + 224
        byte <TILE_MEM + 240
        byte <TILE_MEM + 256
        byte <TILE_MEM + 272
        byte <TILE_MEM + 288
        byte <TILE_MEM + 304
        byte <TILE_MEM + 320            ; 20
        byte <TILE_MEM + 336
        byte <TILE_MEM + 352
        byte <TILE_MEM + 368
        byte <TILE_MEM + 384
        byte <TILE_MEM + 400
        byte <TILE_MEM + 416
        byte <TILE_MEM + 432
        byte <TILE_MEM + 448
        byte <TILE_MEM + 464
        byte <TILE_MEM + 480            ; 30
        byte <TILE_MEM + 496
        byte <TILE_MEM + 512
        byte <TILE_MEM + 528
        byte <TILE_MEM + 544
        byte <TILE_MEM + 560            
        byte <TILE_MEM + 576
        byte <TILE_MEM + 592
        byte <TILE_MEM + 608
        byte <TILE_MEM + 624
        byte <TILE_MEM + 640            ; 40
        byte <TILE_MEM + 656
        byte <TILE_MEM + 672
        byte <TILE_MEM + 688
        byte <TILE_MEM + 704
        byte <TILE_MEM + 720            
        byte <TILE_MEM + 736
        byte <TILE_MEM + 752
        byte <TILE_MEM + 768
        byte <TILE_MEM + 784
        byte <TILE_MEM + 800            ;50
        byte <TILE_MEM + 816
        byte <TILE_MEM + 832
        byte <TILE_MEM + 848
        byte <TILE_MEM + 864
        byte <TILE_MEM + 880
        byte <TILE_MEM + 896
        byte <TILE_MEM + 912
        byte <TILE_MEM + 928
        byte <TILE_MEM + 944
        byte <TILE_MEM + 960            ; 10
        byte <TILE_MEM + 976
        byte <TILE_MEM + 992
        byte <TILE_MEM + 1008
        byte <TILE_MEM + 1024           ; 64

TILE_NUMBER_LOOKUP_HI
        byte >TILE_MEM                  ; 0
        byte >TILE_MEM + 16
        byte >TILE_MEM + 32
        byte >TILE_MEM + 48
        byte >TILE_MEM + 64
        byte >TILE_MEM + 80
        byte >TILE_MEM + 96
        byte >TILE_MEM + 112
        byte >TILE_MEM + 128
        byte >TILE_MEM + 144
        byte >TILE_MEM + 160            ; 10
        byte >TILE_MEM + 176
        byte >TILE_MEM + 192
        byte >TILE_MEM + 208
        byte >TILE_MEM + 224
        byte >TILE_MEM + 240
        byte >TILE_MEM + 256
        byte >TILE_MEM + 272
        byte >TILE_MEM + 288
        byte >TILE_MEM + 304
        byte >TILE_MEM + 320            ; 20
        byte >TILE_MEM + 336
        byte >TILE_MEM + 352
        byte >TILE_MEM + 368
        byte >TILE_MEM + 384
        byte >TILE_MEM + 400
        byte >TILE_MEM + 416
        byte >TILE_MEM + 432
        byte >TILE_MEM + 448
        byte >TILE_MEM + 464
        byte >TILE_MEM + 480            ; 30
        byte >TILE_MEM + 496
        byte >TILE_MEM + 512
        byte >TILE_MEM + 528
        byte >TILE_MEM + 544
        byte >TILE_MEM + 560            
        byte >TILE_MEM + 576
        byte >TILE_MEM + 592
        byte >TILE_MEM + 608
        byte >TILE_MEM + 624
        byte >TILE_MEM + 640            ; 40
        byte >TILE_MEM + 656
        byte >TILE_MEM + 672
        byte >TILE_MEM + 688
        byte >TILE_MEM + 704
        byte >TILE_MEM + 720            
        byte >TILE_MEM + 736
        byte >TILE_MEM + 752
        byte >TILE_MEM + 768
        byte >TILE_MEM + 784
        byte >TILE_MEM + 800            ;50
        byte >TILE_MEM + 816
        byte >TILE_MEM + 832
        byte >TILE_MEM + 848
        byte >TILE_MEM + 864
        byte >TILE_MEM + 880
        byte >TILE_MEM + 896
        byte >TILE_MEM + 912
        byte >TILE_MEM + 928
        byte >TILE_MEM + 944
        byte >TILE_MEM + 960            ; 10
        byte >TILE_MEM + 976
        byte >TILE_MEM + 992
        byte >TILE_MEM + 1008

TILE2_NUMBER_LOOKUP_LO
        byte <TILE2_MEM                  ; 0
        byte <TILE2_MEM + 16
        byte <TILE2_MEM + 32
        byte <TILE2_MEM + 48
        byte <TILE2_MEM + 64
        byte <TILE2_MEM + 80
        byte <TILE2_MEM + 96
        byte <TILE2_MEM + 112
        byte <TILE2_MEM + 128
        byte <TILE2_MEM + 144
        byte <TILE2_MEM + 160            ; 10
        byte <TILE2_MEM + 176
        byte <TILE2_MEM + 192
        byte <TILE2_MEM + 208
        byte <TILE2_MEM + 224
        byte <TILE2_MEM + 240
        byte <TILE2_MEM + 256
        byte <TILE2_MEM + 272
        byte <TILE2_MEM + 288
        byte <TILE2_MEM + 304
        byte <TILE2_MEM + 320            ; 20
        byte <TILE2_MEM + 336
        byte <TILE2_MEM + 352
        byte <TILE2_MEM + 368
        byte <TILE2_MEM + 384
        byte <TILE2_MEM + 400
        byte <TILE2_MEM + 416
        byte <TILE2_MEM + 432
        byte <TILE2_MEM + 448
        byte <TILE2_MEM + 464
        byte <TILE2_MEM + 480            ; 30
        byte <TILE2_MEM + 496
        byte <TILE2_MEM + 512
        byte <TILE2_MEM + 528
        byte <TILE2_MEM + 544
        byte <TILE2_MEM + 560            
        byte <TILE2_MEM + 576
        byte <TILE2_MEM + 592
        byte <TILE2_MEM + 608
        byte <TILE2_MEM + 624
        byte <TILE2_MEM + 640            ; 40
        byte <TILE2_MEM + 656
        byte <TILE2_MEM + 672
        byte <TILE2_MEM + 688
        byte <TILE2_MEM + 704
        byte <TILE2_MEM + 720            
        byte <TILE2_MEM + 736
        byte <TILE2_MEM + 752
        byte <TILE2_MEM + 768
        byte <TILE2_MEM + 784
        byte <TILE2_MEM + 800            ;50
        byte <TILE2_MEM + 816
        byte <TILE2_MEM + 832
        byte <TILE2_MEM + 848
        byte <TILE2_MEM + 864
        byte <TILE2_MEM + 880
        byte <TILE2_MEM + 896
        byte <TILE2_MEM + 912
        byte <TILE2_MEM + 928
        byte <TILE2_MEM + 944
        byte <TILE2_MEM + 960            ; 10
        byte <TILE2_MEM + 976
        byte <TILE2_MEM + 992
        byte <TILE2_MEM + 1008
        byte <TILE2_MEM + 1024           ; 64

TILE2_NUMBER_LOOKUP_HI

        byte >TILE2_MEM                  ; 0
        byte >TILE2_MEM + 16
        byte >TILE2_MEM + 32
        byte >TILE2_MEM + 48
        byte >TILE2_MEM + 64
        byte >TILE2_MEM + 80
        byte >TILE2_MEM + 96
        byte >TILE2_MEM + 112
        byte >TILE2_MEM + 128
        byte >TILE2_MEM + 144
        byte >TILE2_MEM + 160            ; 10
        byte >TILE2_MEM + 176
        byte >TILE2_MEM + 192
        byte >TILE2_MEM + 208
        byte >TILE2_MEM + 224
        byte >TILE2_MEM + 240
        byte >TILE2_MEM + 256
        byte >TILE2_MEM + 272
        byte >TILE2_MEM + 288
        byte >TILE2_MEM + 304
        byte >TILE2_MEM + 320            ; 20
        byte >TILE2_MEM + 336
        byte >TILE2_MEM + 352
        byte >TILE2_MEM + 368
        byte >TILE2_MEM + 384
        byte >TILE2_MEM + 400
        byte >TILE2_MEM + 416
        byte >TILE2_MEM + 432
        byte >TILE2_MEM + 448
        byte >TILE2_MEM + 464
        byte >TILE2_MEM + 480            ; 30
        byte >TILE2_MEM + 496
        byte >TILE2_MEM + 512
        byte >TILE2_MEM + 528
        byte >TILE2_MEM + 544
        byte >TILE2_MEM + 560            
        byte >TILE2_MEM + 576
        byte >TILE2_MEM + 592
        byte >TILE2_MEM + 608
        byte >TILE2_MEM + 624
        byte >TILE2_MEM + 640            ; 40
        byte >TILE2_MEM + 656
        byte >TILE2_MEM + 672
        byte >TILE2_MEM + 688
        byte >TILE2_MEM + 704
        byte >TILE2_MEM + 720            
        byte >TILE2_MEM + 736
        byte >TILE2_MEM + 752
        byte >TILE2_MEM + 768
        byte >TILE2_MEM + 784
        byte >TILE2_MEM + 800            ;50
        byte >TILE2_MEM + 816
        byte >TILE2_MEM + 832
        byte >TILE2_MEM + 848
        byte >TILE2_MEM + 864
        byte >TILE2_MEM + 880
        byte >TILE2_MEM + 896
        byte >TILE2_MEM + 912
        byte >TILE2_MEM + 928
        byte >TILE2_MEM + 944
        byte >TILE2_MEM + 960            ; 10
        byte >TILE2_MEM + 976
        byte >TILE2_MEM + 992
        byte >TILE2_MEM + 1008
        byte >TILE2_MEM + 1024           ; 64
                                                                             