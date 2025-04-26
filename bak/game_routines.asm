;===============================================================================
; GAME CORE ROUTINES
;===============================================================================
; Core routines for the framework - Peter 'Sig' Hewett
; 2016
;-------------------------------------------------------------------------------
; Wait for the raster to reach line $f8 - if it's aleady there, wait for
; the next screen blank. This prevents mistimings if the code runs too fast
#region "WaitFrame"
WaitFrame
        lda VIC_RASTER_LINE         ; fetch the current raster line
        cmp #$F8                    ; wait here till l        
        beq WaitFrame           
        
@WaitStep2
        lda VIC_RASTER_LINE
        cmp #$F8
        bne @WaitStep2
        rts
#endregion        
;-------------------------------------------------------------------------------
; UPDATE TIMERS
;-------------------------------------------------------------------------------
; 2 basic timers - a fast TIMER that is updated every frame,
; and a SLOW_TIMER updated every 16 frames
;-------------------------------------------------------------------------------
#region "UpdateTimers"
UpdateTimers
        inc TIMER                       ; increment TIMER by 1
        lda TIMER
        and #$0F                        ; check if it's equal to 16
        beq @updateSlowTimer            ; if so we update SLOW_TIMER        
        rts

@updateSlowTimer
        inc SLOW_TIMER                  ; increment slow timer
        rts

DetectKeyPress
        lda #%11111110         ; pull row 0 low (SPACE is in row 0, bit 4)
        sta $dc00
        lda $dc01              ; read column
        and #%00010000         ; bit 4 = 0 when SPACE is pressed
        bne no_key             ; if bit is 1, not pressed

        lda #1
        sta $d020              ; change border color if SPACE is pressed

no_key
        rts        


LoadColors
            ldx #0
LoadColorsLoop
            ;lda color_data, x
            lda ATTRIBUTE2_MEM,x
            sta $D800,x  ; Write to color RAM
            inx
            cpx $03E8    ; 1000 bytes
            bne LoadColorsLoop
            rts

LoadScreen
            ldx #0
LoadScreenLoop
            ;lda screen_data, x
            lda TILE_MEM,x
            sta $0400,x  ; Write to screen RAM
            inx
            cpx $03E8    ; 1000 bytes (40x25 screen)
            bne LoadScreenLoop
            rts

; Load in our character sets:

; = 0: Hotel
; = 1: Alleyway
LoadAlleyMap
        ldx #0                      ; Initialize page counter to 0
        ldy #0                      ; Initialize byte counter within page to 0


; = 0 - Hotel Map
; = 1 - Alleyway Map

;        lda maptwoloaded
;        bne @loadalleymap2          ;  = 1: Load Alleyway map

;        lda #<MAP_CHAR_MEM
;        sta ZEROPAGE_POINTER_4
;        lda #>MAP_CHAR_MEM
;        sta ZEROPAGE_POINTER_4 + 1
;        jmp @skipalleymap

@loadalleymap2
        lda #<MAP2_CHAR_MEM
        sta ZEROPAGE_POINTER_4
        lda #>MAP2_CHAR_MEM
        sta ZEROPAGE_POINTER_4 + 1

; MAP2_CHAR_MEM: at $7c29 (31785), 41, 124

@skipalleymap2
        lda #0
        sta ZEROPAGE_POINTER_2       ; Initialize low byte of the pointer (start address low byte)
        lda #124
        sta ZEROPAGE_POINTER_2 + 1   ; Initialize high byte of the pointer (start address high byte)

copy_loopforalley
        lda (ZEROPAGE_POINTER_4),y         ; Load byte from MAP2_CHAR_MEM
        sta (ZEROPAGE_POINTER_2),y  ; Store it in the address pointed by ZEROPAGE_POINTER_2 + Y
        iny                          ; Increment Y (move to the next byte)

        bne continue_copyalley            ; If Y is not zero, continue copying

        inc ZEROPAGE_POINTER_2 + 1   ; If Y is zero, increment the high byte of the pointer
        inc ZEROPAGE_POINTER_4 + 1
        inx                          ; Increment X (page counter)

        cpx #8                     ; Check if 7 full pages (1792 bytes) have been copied
        beq last_partalley                ; If so, handle the remaining 240 bytes

continue_copyalley
        jmp copy_loopforalley                ; Jump back to continue copying

last_partalley
        lda #0                       ; Reset Y to 0 for the last part
        ldy #0                       ; Reset Y to 0 for byte copy (copy the remaining 240 bytes)
        
        ; Copy the remaining 240 bytes
        ; Note: We are already at $4800 + 1792 bytes, so copy the last 240 bytes
        cpx #9                     ; Ensure we're beyond the 7 full pages
        bcc done2                     ; If done copying 2032 bytes, we exit

        ; We copy the last 240 bytes
        ; We can now jump back into copying the final 240 bytes.
        jmp copy_loopforalley              ; Continue copying from the last remaining bytes
        
done2
        rts   


LoadHotelMap
        ldx #0                      ; Initialize page counter to 0
        ldy #0                      ; Initialize byte counter within page to 0


; = 0 - Hotel Map
; = 1 - Alleyway Map

        lda #<MAP_CHAR_MEM
        sta ZEROPAGE_POINTER_4
        lda #>MAP_CHAR_MEM
        sta ZEROPAGE_POINTER_4 + 1
        jmp @skipalleymap

; 0, $48

@skipalleymap
        lda #0
        sta ZEROPAGE_POINTER_2       ; Initialize low byte of the pointer (start address low byte)
        lda #124
        sta ZEROPAGE_POINTER_2 + 1   ; Initialize high byte of the pointer (start address high byte)

copy_loopset2
        lda (ZEROPAGE_POINTER_4),y         ; Load byte from MAP2_CHAR_MEM
        sta (ZEROPAGE_POINTER_2),y  ; Store it in the address pointed by ZEROPAGE_POINTER_2 + Y
        iny                          ; Increment Y (move to the next byte)

        bne continue_copy2            ; If Y is not zero, continue copying

        inc ZEROPAGE_POINTER_2 + 1   ; If Y is zero, increment the high byte of the pointer
        inc ZEROPAGE_POINTER_4 + 1
        inx                          ; Increment X (page counter)

        cpx #$07                     ; Check if 7 full pages (1792 bytes) have been copied
        beq last_part2                ; If so, handle the remaining 240 bytes

continue_copy2
        jmp copy_loopset2                ; Jump back to continue copying

last_part2
        lda #0                       ; Reset Y to 0 for the last part
        ldy #0                       ; Reset Y to 0 for byte copy (copy the remaining 240 bytes)
        
        ; Copy the remaining 240 bytes
        ; Note: We are already at $4800 + 1792 bytes, so copy the last 240 bytes
        cpx #$09                     ; Ensure we're beyond the 7 full pages
        bcc done                     ; If done copying 2032 bytes, we exit

        ; We copy the last 240 bytes
        ; We can now jump back into copying the final 240 bytes.
        jmp copy_loopset2              ; Continue copying from the last remaining bytes
        
done
        rts                          ; Return from subroutine                       ; Return from subroutine


LoadNewTiles
        ldx #0
       
; Set up zero-page pointer
        lda #<TILE_MEM
        sta ZEROPAGE_POINTER_2
        LDA #>TILE_MEM
        sta ZEROPAGE_POINTER_2 + 1

; Copy 512 bytes from TilesData to zero-page ($20 - $220)

copy_outer_loop
        ldy #0


Load_Tiles
        lda (ZEROPAGE_POINTER_2),y
        sta $20,y
        iny
        cpy #$10        ; 16 bytes per inner loop (32 * 16 bytes = 512 total)
        bne Load_Tiles
        inc ZEROPAGE_POINTER_2 + 1
        cpy #$10        ; If 256 bytes have been copied (16 * 16), break
        bne copy_outer_loop
        rts

LoadAlleyTiles
        ldx #0
       
; Set up zero-page pointer
        lda #<TILE2_MEM
        sta ZEROPAGE_POINTER_2
        LDA #>TILE2_MEM
        sta ZEROPAGE_POINTER_2 + 1

; Copy 512 bytes from TilesData to zero-page ($20 - $220)

copy_alley_tiles
        ldy #0


Load_AlleyTiles
        lda (ZEROPAGE_POINTER_2),y
        sta 32,y
        iny
        cpy #22        ; 16 bytes per inner loop (32 * 16 bytes = 512 total)
        bne Load_Tiles
        inc ZEROPAGE_POINTER_2 + 1
        cpy #22        ; If 256 bytes have been copied (16 * 16), break
        bne copy_alley_tiles
        rts

; Tomorrow 4/24/25: Homework

; Instead of referencing the tables in main.asm
; We we now access the Memory Addresses!

; Set up ATTRIBUTE = $5600
; Set up MAP_MEM = $8000
; Set up TILE_MEM = $9000

LoadChsetFromDisk
        lda LOAD_ADDRESS_TABLE_LO,x
        sta addrlo
        lda LOAD_ADDRESS_TABLE_HI,x
        sta addrhi

        lda FILE_LENGTH_TABLE,x
        sta flength

        lda FILENAME_TABLE_LO,x    ; read filename(x)
        sta filelo

        lda FILENAME_TABLE_HI,x    ; read filename(x)
        sta filehi

        lda flength,x
        ldx filelo
        ldy filehi
        jsr $ffbd           ; SETNAM

        lda #$00            ; Logical file number
        ldx #$08            ; Device 8 (typical for disk drive)
        ldy #$00            ; Secondary address (used for loading)
        jsr $ffba           ; SETLFS

        lda #$00
        ldx addrlo
        ldy addrhi

        jsr $ffd5           ; LOAD (KERNAL)
        bcs file_not_found

;        lda #3
;        sta hotelchars

        lda #7
        sta 53280
        rts

LoadChsetFromDisk2
        lda LOAD_ADDRESS_TABLE_LO,x
        sta addrlo
        lda LOAD_ADDRESS_TABLE_HI,x
        sta addrhi

        lda FILE_LENGTH_TABLE,x
        sta flength

        lda FILENAME_TABLE_LO,x    ; read filename(x)
        sta filelo

        lda FILENAME_TABLE_HI,x    ; read filename(x)
        sta filehi

        lda flength,x
        ldx filelo
        ldy filehi
        jsr $ffbd           ; SETNAM

        lda #$00            ; Logical file number
        ldx #$08            ; Device 8 (typical for disk drive)
        ldy #$00            ; Secondary address (used for loading)
        jsr $ffba           ; SETLFS

        lda #$00
        ldx addrlo
        ldy addrhi

        jsr $ffd5           ; LOAD (KERNAL)
        bcs file_not_found

        lda #7
        sta 53280
        rts

file_not_found
        lda #2
        sta 53820
        rts

filename1 byte "hotelchars.bin" 
filename2 byte "hotel4map.bin"
filename3 byte "hotel1attrib.bin"
filename4 byte "hotel4tiles.bin"
filename5 byte "alley4chars.bin"

flength byte 0
filelo byte 0
filehi byte 0

LOAD_ADDRESS_TABLE_LO byte <charload,<MAP_MEM,<ATTRIBUTE_MEM,<TILE_MEM,<charload
LOAD_ADDRESS_TABLE_HI byte >charload,>MAP_MEM,>ATTRIBUTE_MEM,>TILE_MEM,>charload

;Four different bytes to select the filename to load
FILENAME_TABLE_LO byte <filename1,<filename2,<filename3,<filename4,<filename5
FILENAME_TABLE_HI byte >filename1,>filename2,>filename3,>filename4,>filename5

charload = $4802
alleymap = $4802
tilesload = $8000
hotelchars byte 0
FILE_LENGTH_TABLE byte 14,13,16,15,15

addrlo byte 0
addrhi byte 0

ClearChsetMemory
        lda #<$4800
        sta ZEROPAGE_POINTER_2
        LDA #>$4800
        sta ZEROPAGE_POINTER_2 + 1

; Copy 512 bytes from TilesData to zero-page ($20 - $220)

clear_chsetdata
        ldx #8
        ldy #0

clearing
        lda #0
        sta (ZEROPAGE_POINTER_2),y
        iny
        bne clearing
        lda ZEROPAGE_POINTER_2 + 1
        inc ZEROPAGE_POINTER_2 + 1
        adc ZEROPAGE_POINTER_2 
        sta ZEROPAGE_POINTER_2
;        lda ZEROPAGE_POINTER_2 + 1
;        sta ZEROPAGE_POINTER_2 + 1
        dex        ; If 256 bytes have been copied (16 * 16), break
        cpx #2
        bne clear_chsetdata
        rts

; Animate a tile

AnimateTiles
        ldx #0                      ; Initialize page counter to 0
        ldy #0                      ; Initialize byte counter within page to 0


; = 0 - Hotel Map
; = 1 - Alleyway Map

@loadmapanimate
        lda #<MAP2_CHAR_MEM
        sta ZEROPAGE_POINTER_4
        lda #>MAP2_CHAR_MEM
        sta ZEROPAGE_POINTER_4 + 1


        lda #0
        sta ZEROPAGE_POINTER_2       ; Initialize low byte of the pointer (start address low byte)
        lda #$48
        sta ZEROPAGE_POINTER_2 + 1   ; Initialize high byte of the pointer (start address high byte)

copy_loopanimate
        lda (ZEROPAGE_POINTER_4),y         ; Load byte from MAP2_CHAR_MEM
        sta (ZEROPAGE_POINTER_2),y  ; Store it in the address pointed by ZEROPAGE_POINTER_2 + Y
        iny                          ; Increment Y (move to the next byte)

        bne continue_copyanim            ; If Y is not zero, continue copying

        inc ZEROPAGE_POINTER_2 + 1   ; If Y is zero, increment the high byte of the pointer
        inc ZEROPAGE_POINTER_4 + 1
        inx                          ; Increment X (page counter)

        cpx #$08                     ; Check if 7 full pages (1792 bytes) have been copied
        beq last_partofanim                ; If so, handle the remaining 240 bytes

continue_copyanim
        jmp copy_loopanimate                ; Jump back to continue copying

last_partofanim
        lda #0                       ; Reset Y to 0 for the last part
        ldy #0                       ; Reset Y to 0 for byte copy (copy the remaining 240 bytes)
        
        ; Copy the remaining 240 bytes
        ; Note: We are already at $4800 + 1792 bytes, so copy the last 240 bytes
        cpx #$09                     ; Ensure we're beyond the 7 full pages
        bcc doneanim                     ; If done copying 2032 bytes, we exit

        ; We copy the last 240 bytes
        ; We can now jump back into copying the final 240 bytes.
        jmp copy_loopanimate              ; Continue copying from the last remaining bytes
        
doneanim
        rts 

AnimateStreetLight
        ldy #0
        lda #<MAP2_CHAR_MEM
        sta ZEROPAGE_POINTER_4
        lda #>MAP2_CHAR_MEM
        sta ZEROPAGE_POINTER_4 + 1

        lda #217
        sta ZEROPAGE_POINTER_2       ; Initialize low byte of the pointer (start address low byte)
        lda #72
        sta ZEROPAGE_POINTER_2 + 1   ; Initialize high byte of the pointer (start address high byte)

copy_lights
        lda #217                     ; Load byte from MAP2_CHAR_MEM
        sta (ZEROPAGE_POINTER_4),y  ; Store it in the address pointed by ZEROPAGE_POINTER_2 + Y
        iny                          ; Increment Y (move to the next byte)
        cpy #8
        bne copy_lights            ; If Y is not zero, continue copying
        rts

;===============================================================================
; ANIMATION TIMER
;===============================================================================
AnimScreenControl
        lda $CB
        cmp #60
        bne @exit
        clc
        adc #1
        lda animLevel
        sta animLevel 
        sta 53280
        rts

@exit
        rts

WhichKey
        lda $CB
        cmp #60
        bne @exitKey
        lda #2
        sta 53280

@exitKey
        rts

;===============================================================================
; TILE DISPLAY (Future)
;===============================================================================
PlotATile
        ldx #70                        ; (129,26=default), 61
        ldy #20                          ; , 27

        jsr TileMap                     ; Draw the level map (Screen1)
                                        ; And initialize it

        jsr CopyToBuffer                ; Copy to the backbuffer(Screen2)
        rts

;===============================================================================
; SCORE PANEL DISPLAY
;===============================================================================
ScoreBoard
        sta PARAM4                                      ; gamescore data
        jsr GetLineAddress

        lda COLOR_LINE_OFFSET_TABLE_LO,x                ; fetch line address for color
        sta ZEROPAGE_POINTER_3
        lda COLOR_LINE_OFFSET_TABLE_HI,x
        sta ZEROPAGE_POINTER_3 + 1

;===============================================================================
; Gamescore counter 0-9 + carry bit (into high nybble)
;===============================================================================
        sed
        clc
        lda gamescore                                   ; increase score
        adc #1                                          ; 01,00
        sta gamescore
        lda gamescore+1
        adc #0                                          ;00, 00
        sta gamescore+1
        lda gamescore+2
        adc #0
        sta gamescore+2
        cld
        jsr display
        rts

display
        ldy #12          ;screen offset
        ldx #0          ; score byte index
sloop
        lda gamescore,x
        pha
        and #$0f        ; count between 0-9
        jsr plotdigit

        pla
        lsr a
        lsr a
        lsr a
        lsr a
        jsr plotdigit
        inx
        cpx #3
        bne sloop
        rts

plotdigit
        clc
        adc #48                                      ; write '0' zero on screen
        sta (ZEROPAGE_POINTER_1),y                   ; write the character code
        lda #COLOR_CYAN                              ; set the color to blue
        sta (ZEROPAGE_POINTER_3),y                   ; write the color to color ram  
        dey
        rts

#endregion
;-------------------------------------------------------------------------------
;  READ JOY 2
;-------------------------------------------------------------------------------
; Trying this a different way this time.  Rather than hitting the joystick 
; registers then
; doing something every time - The results will be stored in JOY_X and JOY_Y 
; with values -1 to 1 , with 0 meaning 'no input' 
; - I should be able to just add this to a 
; sprite for a
; simple move, while still being able to do an easy check for more complicated 
; movement later on
;-------------------------------------------------------------------------------
#region "ReadJoystick"

ReadJoystick
        lda #$00                        ; Reset JOY X and Y variables
        sta JOY_X
        sta JOY_Y
        sta NE_DIR
@testUp                                 ; Test for Up pressed
        lda checkup                     ; Mask for bit 0
        bit JOY_2                       ; test bit 0 for press
        bne @testDown
        lda #$FF                        ; set JOY_Y to -1 ($FF)
        sta JOY_Y
        jmp @testLeft                   ; Can't be up AND down

@testDown                               ; Test for Down
        lda checkdown                   ; Mask for bit 1
        bit JOY_2
        bne @testLeft
        lda #$01                        ; set JOY_Y to 1 ($01)
        sta JOY_Y
        rts
@testLeft                               ; Test for Left
        lda checkleft                   ; Mask for bit 2
        bit JOY_2
        bne @testRight
        lda #$FF
        sta JOY_X
        rts                             ; Can't be left AND right - no more tests

@testRight                              ; Test for Right
        lda checkright                  ; Mask for bit 3
        bit JOY_2
        bne @checkUpLeft
        lda #$01
        sta JOY_X
        rts   

@checkUpLeft                            ; check zero = button pressed
        lda #%00010000
        bit JOY_2                       ; check zero = button pressed
        bne @testDownRight              ; continue other checks

@testUpLeft
        lda #1
        sta NE_DIR
        rts

@testDownRight                          ; Test for Right
        lda checkdownright              ; Mask for bit 3
        bit JOY_2
        bne @done
        lda #$02
        sta NE_DIR
        rts 

@done    
        rts

#endregion
;-------------------------------------------------------------------------------
; JOYSTICK BUTTON PRESSED
;-------------------------------------------------------------------------------
; Notifies the state of the fire button on JOYSTICK 2.
; BUTTON_ACTION is set to one on a single press 
; (that is when the button is released)
; BUTTON_PRESSED is set to 1 while the button is held down.
; So either a long press, or a single press can be accounted for.
; TODO I might put a 'press counter' in here to test how long the button is 
; down for..
;-------------------------------------------------------------------------------
#region "JoyButton"

JoyButton

        lda #1                                  ; checks for a previous button action
        cmp BUTTON_ACTION                       ; and clears it if set
        bne @buttonTest

        lda #0                                  
        sta BUTTON_ACTION

@buttonTest
        lda #$10                                ; test bit #4 in JOY_2 Register
        bit JOY_2
        bne @buttonNotPressed
        
        lda #1                                  ; if it's pressed - save the result
        sta BUTTON_PRESSED                      ; and return - we want a single press
        rts                                     ; so we need to wait for the release

@buttonNotPressed

        lda BUTTON_PRESSED                      ; and check to see if it was pressed first
        bne @buttonAction                       ; if it was we go and set BUTTON_ACTION
        rts

@buttonAction
        lda #0
        sta BUTTON_PRESSED
        lda #1
        sta BUTTON_ACTION

        rts

#endregion        

;-------------------------------------------------------------------------------
; COPY CHARACTER SET
;-------------------------------------------------------------------------------
; Copy the custom character set into the VIC Memory Bank (2048 bytes)
; ZEROPAGE_POINTER_1 = Source
; ZEROPAGE_POINTER_2 = Dest
;
; Returns A,X,Y and PARAM2 intact
;-------------------------------------------------------------------------------

#region "CopyChars"

CopyChars
        
        saveRegs

        ldx #$00                                ; clear X, Y, A and PARAM2
        ldy #$00
        lda #$00
        sta PARAM2
@NextLine

; CHAR_MEM = ZEROPAGE_POINTER_1
; LEVEL_1_CHARS = ZEROPAGE_POINTER_2

        lda (ZEROPAGE_POINTER_1),Y              ; copy from source to target
        sta (ZEROPAGE_POINTER_2),Y

        inx                                     ; increment x / y
        iny                                     
        cpx #$08                                ; test for next character block (8 bytes)
        bne @NextLine                           ; copy next line
        cpy #$00                                ; test for edge of page (256 wraps back to 0)
        bne @PageBoundryNotReached

        inc ZEROPAGE_POINTER_1 + 1              ; if reached 256 bytes, increment high byte
        inc ZEROPAGE_POINTER_2 + 1              ; of source and target

@PageBoundryNotReached
        inc PARAM2                              ; Only copy 254 characters (to keep irq vectors intact)
        lda PARAM2                              ; If copying to F000-FFFF block
        cmp #255
        beq @CopyCharactersDone
        ldx #$00
        jmp @NextLine

@CopyCharactersDone

        restoreRegs

        rts
#endregion

; y=210 - stone wall scrolls
; y=186 - bottom of water tile (top part)
; y=194 (current)

;===============================================================================
; WATER ANIMATION FRAMES
;===============================================================================
WaterAnimation
        lda RIVER_ANIM3_LO
        sta ZEROPAGE_POINTER_3
        lda RIVER_ANIM3_HI
        sta ZEROPAGE_POINTER_3 + 1

@contAnim
        ldy #0 
        ldx #0                

; Still looping the animation
@shiftPixelsRight
; Get all 8 bits (128,64,32,16,8,4,2,1)

@store2
        lda CHRADR3,y
        lsr a
        bcc @store5
        clc        
        adc #128                 ; shift pixels down
;;        
@store5
        sta (ZEROPAGE_POINTER_3),y
        iny

; This comparison checks for the tile width + (all 4 tiles)
; In a 4x4 matrix. Because 4 x 4 = 16
        cpy #48                         ;58                                         
        bcc @shiftPixelsRight
        rts

ReadCharsetAddress
@screen1
;        lda MAPSCREEN1_CHSET_OFFSET_TABLE_LO,y  ; Use Y to lookup the address and save it in
        sta ZEROPAGE_POINTER_4                  ; ZEROPAGE_POINTER_1
;        lda MAPSCREEN1_CHSET_OFFSET_TABLE_HI,y
        sta ZEROPAGE_POINTER_4 + 1
        rts

riverAnimation
        clc
        lda waterSpeed                           ; increase score
        adc #1                                   ; 01,00
        sta waterSpeed
        
        lda waterSpeed
        cmp #40
        bcc @exitLoop

        lda CURRENT_SCREEN + 1          ; Hi byte of the current screen
        cmp #>SCREEN2_MEM               ; compare to start of Screen2
        beq @screen2_scene1

        lda #102                  ; Set VIC to Screen0, Charset 1
        sta VIC_MEMORY_CONTROL
        lda #5
        sta 53280
        jmp @scrollwater1

@screen2_scene1          
        lda #2                  ; Set VIC to Screen1, Charset 1
        sta VIC_MEMORY_CONTROL
        lda #3
        sta 53280

@scrollwater1       
        lda waterSpeed
        cmp #80
        bcc @exitLoop

        lda CURRENT_SCREEN + 1          ; Hi byte of the current screen
        cmp #>SCREEN2_MEM               ; compare to start of Screen2
        beq @screen2_scene2

        lda #102
        sta VIC_MEMORY_CONTROL
        lda #2
        sta 53280
        jmp @scrollwater2

@screen2_scene2          
        lda #102                 ; Set VIC to Screen1, Charset 1
        sta VIC_MEMORY_CONTROL
        lda #7
        sta 53280

@scrollwater2
        lda waterSpeed
        cmp #120
        bcc @exitLoop

        lda #0
        sta waterSpeed

@exitLoop
        rts

riverAnimation2
        clc
        lda waterSpeed                 ; increase score
        adc #1                         ; 01,00
        sta waterSpeed
        
        lda waterSpeed
        cmp #30
        bcc @exitLoop

        lda #20                         ; 20 = $5000 - Parkour Redo ChsetNew1.bin"
        sta VIC_MEMORY_CONTROL  
      
        lda waterSpeed
        cmp #70
        bcc @exitLoop

        lda waterSpeed
        cmp #110
        bcc @exitLoop

        lda #0
        sta waterSpeed

@exitLoop
        rts


;MAPSCREEN1_CHSET_OFFSET_TABLE_LO
;        byte <MAP_CHAR_MEM
;        byte <MAP_CHAR_MEM + 8
;        byte <MAP_CHAR_MEM + 16
;        byte <MAP_CHAR_MEM + 24
;        byte <MAP_CHAR_MEM + 32
;        byte <MAP_CHAR_MEM + 40
;        byte <MAP_CHAR_MEM + 48
;        byte <MAP_CHAR_MEM + 56
;        byte <MAP_CHAR_MEM + 64
;        byte <MAP_CHAR_MEM + 72
;        byte <MAP_CHAR_MEM + 80
;        byte <MAP_CHAR_MEM + 88
;        byte <MAP_CHAR_MEM + 96
;        byte <MAP_CHAR_MEM + 104
;        byte <MAP_CHAR_MEM + 112
;        byte <MAP_CHAR_MEM + 120
;        byte <MAP_CHAR_MEM + 128
;        byte <MAP_CHAR_MEM + 136
;        byte <MAP_CHAR_MEM + 144
;        byte <MAP_CHAR_MEM + 152
;        byte <MAP_CHAR_MEM + 160
;        byte <MAP_CHAR_MEM + 168
;        byte <MAP_CHAR_MEM + 176
;        byte <MAP_CHAR_MEM + 184
;        byte <MAP_CHAR_MEM + 192
;        byte <MAP_CHAR_MEM + 200
;        byte <MAP_CHAR_MEM + 208
;        byte <MAP_CHAR_MEM + 216
;        byte <MAP_CHAR_MEM + 224
;        byte <MAP_CHAR_MEM + 232
;        byte <MAP_CHAR_MEM + 240
;        byte <MAP_CHAR_MEM + 248
;        byte <MAP_CHAR_MEM + 256
;        byte <MAP_CHAR_MEM + 264
;        byte <MAP_CHAR_MEM + 272        
;        byte <MAP_CHAR_MEM + 280
;        byte <MAP_CHAR_MEM + 288
;        byte <MAP_CHAR_MEM + 296
;        byte <MAP_CHAR_MEM + 304
;        byte <MAP_CHAR_MEM + 312
;        byte <MAP_CHAR_MEM + 320        ;40
;        byte <MAP_CHAR_MEM + 328
;        byte <MAP_CHAR_MEM + 336
;        byte <MAP_CHAR_MEM + 344
;        byte <MAP_CHAR_MEM + 352
;        byte <MAP_CHAR_MEM + 360
;        byte <MAP_CHAR_MEM + 368
;        byte <MAP_CHAR_MEM + 376
;        byte <MAP_CHAR_MEM + 384        
;        byte <MAP_CHAR_MEM + 392
;        byte <MAP_CHAR_MEM + 400        ;50
;        byte <MAP_CHAR_MEM + 408
;        byte <MAP_CHAR_MEM + 416
;        byte <MAP_CHAR_MEM + 424
;        byte <MAP_CHAR_MEM + 432
;        byte <MAP_CHAR_MEM + 440
;        byte <MAP_CHAR_MEM + 448
;        byte <MAP_CHAR_MEM + 456        
;        byte <MAP_CHAR_MEM + 464
;        byte <MAP_CHAR_MEM + 472
;        byte <MAP_CHAR_MEM + 480
;        byte <MAP_CHAR_MEM + 488
;        byte <MAP_CHAR_MEM + 496
;        byte <MAP_CHAR_MEM + 504
;        byte <MAP_CHAR_MEM + 512
;        byte <MAP_CHAR_MEM + 520
;        byte <MAP_CHAR_MEM + 528
;        byte <MAP_CHAR_MEM + 536
;        byte <MAP_CHAR_MEM + 544
;        byte <MAP_CHAR_MEM + 552
;        byte <MAP_CHAR_MEM + 560        ;70
;        byte <MAP_CHAR_MEM + 568
;        byte <MAP_CHAR_MEM + 576
;        byte <MAP_CHAR_MEM + 584
;        byte <MAP_CHAR_MEM + 592
;        byte <MAP_CHAR_MEM + 600
;        byte <MAP_CHAR_MEM + 608
;        byte <MAP_CHAR_MEM + 616
;        byte <MAP_CHAR_MEM + 624
;        byte <MAP_CHAR_MEM + 632        
;        byte <MAP_CHAR_MEM + 640
;        byte <MAP_CHAR_MEM + 648
;        byte <MAP_CHAR_MEM + 656
;        byte <MAP_CHAR_MEM + 664        
;        byte <MAP_CHAR_MEM + 672
;        byte <MAP_CHAR_MEM + 680
;        byte <MAP_CHAR_MEM + 688
;        byte <MAP_CHAR_MEM + 696
;        byte <MAP_CHAR_MEM + 704
;        byte <MAP_CHAR_MEM + 712
;        byte <MAP_CHAR_MEM + 720        ;90
;        byte <MAP_CHAR_MEM + 728
;        byte <MAP_CHAR_MEM + 736
;        byte <MAP_CHAR_MEM + 744
;        byte <MAP_CHAR_MEM + 752
;        byte <MAP_CHAR_MEM + 760
;        byte <MAP_CHAR_MEM + 768
;        byte <MAP_CHAR_MEM + 776
;        byte <MAP_CHAR_MEM + 784
;        byte <MAP_CHAR_MEM + 792
;        byte <MAP_CHAR_MEM + 800        ;100
;        byte <MAP_CHAR_MEM + 808
;        byte <MAP_CHAR_MEM + 816
;        byte <MAP_CHAR_MEM + 824
;        byte <MAP_CHAR_MEM + 832
;        byte <MAP_CHAR_MEM + 840
;        byte <MAP_CHAR_MEM + 848
;        byte <MAP_CHAR_MEM + 856
;        byte <MAP_CHAR_MEM + 864
;        byte <MAP_CHAR_MEM + 872
;        byte <MAP_CHAR_MEM + 880        ;110
;        byte <MAP_CHAR_MEM + 888
;        byte <MAP_CHAR_MEM + 896
;        byte <MAP_CHAR_MEM + 904
;        byte <MAP_CHAR_MEM + 912
;        byte <MAP_CHAR_MEM + 920
;        byte <MAP_CHAR_MEM + 928
;        byte <MAP_CHAR_MEM + 936
;        byte <MAP_CHAR_MEM + 944        
;        byte <MAP_CHAR_MEM + 952
;        byte <MAP_CHAR_MEM + 960        ;120
;        byte <MAP_CHAR_MEM + 968
;        byte <MAP_CHAR_MEM + 976
;        byte <MAP_CHAR_MEM + 984
;        byte <MAP_CHAR_MEM + 992
;        byte <MAP_CHAR_MEM + 1000
;        byte <MAP_CHAR_MEM + 1008
;        byte <MAP_CHAR_MEM + 1016        
;        byte <MAP_CHAR_MEM + 1024
;        byte <MAP_CHAR_MEM + 1032
;        byte <MAP_CHAR_MEM + 1040       ;130
;        byte <MAP_CHAR_MEM + 1048
;        byte <MAP_CHAR_MEM + 1056
;        byte <MAP_CHAR_MEM + 1064
;        byte <MAP_CHAR_MEM + 1072
;        byte <MAP_CHAR_MEM + 1080
;        byte <MAP_CHAR_MEM + 1088
;        byte <MAP_CHAR_MEM + 1096
;        byte <MAP_CHAR_MEM + 1104
;        byte <MAP_CHAR_MEM + 1112
;        byte <MAP_CHAR_MEM + 1120       ;140
;        byte <MAP_CHAR_MEM + 1128
;        byte <MAP_CHAR_MEM + 1136
;        byte <MAP_CHAR_MEM + 1144
;        byte <MAP_CHAR_MEM + 1152
;        byte <MAP_CHAR_MEM + 1160    
;        byte <MAP_CHAR_MEM + 1168
;        byte <MAP_CHAR_MEM + 1176
;        byte <MAP_CHAR_MEM + 1184
;        byte <MAP_CHAR_MEM + 1192
;        byte <MAP_CHAR_MEM + 1200       ;150
;        byte <MAP_CHAR_MEM + 1208
;        byte <MAP_CHAR_MEM + 1216
;        byte <MAP_CHAR_MEM + 1224
;        byte <MAP_CHAR_MEM + 1232
;        byte <MAP_CHAR_MEM + 1240
;        byte <MAP_CHAR_MEM + 1248
;        byte <MAP_CHAR_MEM + 1256
;        byte <MAP_CHAR_MEM + 1264
;        byte <MAP_CHAR_MEM + 1272
;        byte <MAP_CHAR_MEM + 1280       ;160
;        byte <MAP_CHAR_MEM + 1288
;        byte <MAP_CHAR_MEM + 1296
;        byte <MAP_CHAR_MEM + 1304
;        byte <MAP_CHAR_MEM + 1312
;        byte <MAP_CHAR_MEM + 1320
;        byte <MAP_CHAR_MEM + 1328
;        byte <MAP_CHAR_MEM + 1336
;        byte <MAP_CHAR_MEM + 1344
;        byte <MAP_CHAR_MEM + 1352       
;        byte <MAP_CHAR_MEM + 1360       ;170
;        byte <MAP_CHAR_MEM + 1368
;        byte <MAP_CHAR_MEM + 1376
;        byte <MAP_CHAR_MEM + 1384
;        byte <MAP_CHAR_MEM + 1392
;        byte <MAP_CHAR_MEM + 1400
;        byte <MAP_CHAR_MEM + 1408
;        byte <MAP_CHAR_MEM + 1416
;        byte <MAP_CHAR_MEM + 1424
;        byte <MAP_CHAR_MEM + 1432
;        byte <MAP_CHAR_MEM + 1440       ;180
;        byte <MAP_CHAR_MEM + 1448
;        byte <MAP_CHAR_MEM + 1456
;        byte <MAP_CHAR_MEM + 1464
;        byte <MAP_CHAR_MEM + 1472
;        byte <MAP_CHAR_MEM + 1480
;        byte <MAP_CHAR_MEM + 1488
;        byte <MAP_CHAR_MEM + 1496
;        byte <MAP_CHAR_MEM + 1504
;        byte <MAP_CHAR_MEM + 1512
;        byte <MAP_CHAR_MEM + 1520       ;190
;        byte <MAP_CHAR_MEM + 1528
;        byte <MAP_CHAR_MEM + 1536
;        byte <MAP_CHAR_MEM + 1544
;        byte <MAP_CHAR_MEM + 1552
;        byte <MAP_CHAR_MEM + 1560       
;        byte <MAP_CHAR_MEM + 1568
;        byte <MAP_CHAR_MEM + 1576
;        byte <MAP_CHAR_MEM + 1584
;        byte <MAP_CHAR_MEM + 1592
;        byte <MAP_CHAR_MEM + 1600       ;200
;        byte <MAP_CHAR_MEM + 1608
;        byte <MAP_CHAR_MEM + 1616
;        byte <MAP_CHAR_MEM + 1624
;        byte <MAP_CHAR_MEM + 1632
;        byte <MAP_CHAR_MEM + 1640
;        byte <MAP_CHAR_MEM + 1648
;        byte <MAP_CHAR_MEM + 1656
;        byte <MAP_CHAR_MEM + 1664
;        byte <MAP_CHAR_MEM + 1672
;        byte <MAP_CHAR_MEM + 1680       ;210
;        byte <MAP_CHAR_MEM + 1688
;        byte <MAP_CHAR_MEM + 1696
;        byte <MAP_CHAR_MEM + 1704
;        byte <MAP_CHAR_MEM + 1712
;        byte <MAP_CHAR_MEM + 1720
;        byte <MAP_CHAR_MEM + 1728
;        byte <MAP_CHAR_MEM + 1736
;        byte <MAP_CHAR_MEM + 1744
;        byte <MAP_CHAR_MEM + 1752
;        byte <MAP_CHAR_MEM + 1760       ;220
;        byte <MAP_CHAR_MEM + 1768
;        byte <MAP_CHAR_MEM + 1776
;        byte <MAP_CHAR_MEM + 1784
;        byte <MAP_CHAR_MEM + 1792
;        byte <MAP_CHAR_MEM + 1800
;        byte <MAP_CHAR_MEM + 1808       ;226
;        byte <MAP_CHAR_MEM + 1816
;        byte <MAP_CHAR_MEM + 1824
;        byte <MAP_CHAR_MEM + 1832
;        byte <MAP_CHAR_MEM + 1840       ;230, 20312
;        byte <MAP_CHAR_MEM + 1848
;        byte <MAP_CHAR_MEM + 1856
;        byte <MAP_CHAR_MEM + 1864
;        byte <MAP_CHAR_MEM + 1872
;        byte <MAP_CHAR_MEM + 1880       ;235

;MAPSCREEN1_CHSET_OFFSET_TABLE_HI
;        byte >MAP_CHAR_MEM
;        byte >MAP_CHAR_MEM + 8
;        byte >MAP_CHAR_MEM + 16
;        byte >MAP_CHAR_MEM + 24
;        byte >MAP_CHAR_MEM + 32
;        byte >MAP_CHAR_MEM + 40
;        byte >MAP_CHAR_MEM + 48
;        byte >MAP_CHAR_MEM + 56
;        byte >MAP_CHAR_MEM + 64
;        byte >MAP_CHAR_MEM + 72
;        byte >MAP_CHAR_MEM + 80
;        byte >MAP_CHAR_MEM + 88
;        byte >MAP_CHAR_MEM + 96
;        byte >MAP_CHAR_MEM + 104
;        byte >MAP_CHAR_MEM + 112
;        byte >MAP_CHAR_MEM + 120
;        byte >MAP_CHAR_MEM + 128
;        byte >MAP_CHAR_MEM + 136
;        byte >MAP_CHAR_MEM + 144
;        byte >MAP_CHAR_MEM + 152
;        byte >MAP_CHAR_MEM + 160
;        byte >MAP_CHAR_MEM + 168
;        byte >MAP_CHAR_MEM + 176
;        byte >MAP_CHAR_MEM + 184
;        byte >MAP_CHAR_MEM + 192
;        byte >MAP_CHAR_MEM + 200
;        byte >MAP_CHAR_MEM + 208
;        byte >MAP_CHAR_MEM + 216
;        byte >MAP_CHAR_MEM + 224
;        byte >MAP_CHAR_MEM + 232
;        byte >MAP_CHAR_MEM + 240
;        byte >MAP_CHAR_MEM + 248
;        byte >MAP_CHAR_MEM + 256
;        byte >MAP_CHAR_MEM + 264
;        byte >MAP_CHAR_MEM + 272        
;        byte >MAP_CHAR_MEM + 280
;        byte >MAP_CHAR_MEM + 288
;        byte >MAP_CHAR_MEM + 296
;        byte >MAP_CHAR_MEM + 304
;        byte >MAP_CHAR_MEM + 312
;        byte >MAP_CHAR_MEM + 320
;        byte >MAP_CHAR_MEM + 328
;        byte >MAP_CHAR_MEM + 336
;        byte >MAP_CHAR_MEM + 344
;        byte >MAP_CHAR_MEM + 352
;        byte >MAP_CHAR_MEM + 360
;        byte >MAP_CHAR_MEM + 368
;        byte >MAP_CHAR_MEM + 376
;        byte >MAP_CHAR_MEM + 384        
;        byte >MAP_CHAR_MEM + 392
;        byte >MAP_CHAR_MEM + 400
;        byte >MAP_CHAR_MEM + 408
;        byte >MAP_CHAR_MEM + 416
;        byte >MAP_CHAR_MEM + 424
;        byte >MAP_CHAR_MEM + 432
;        byte >MAP_CHAR_MEM + 440
;        byte >MAP_CHAR_MEM + 448
;        byte >MAP_CHAR_MEM + 456        
;        byte >MAP_CHAR_MEM + 464
;        byte >MAP_CHAR_MEM + 472
;        byte >MAP_CHAR_MEM + 480
;        byte >MAP_CHAR_MEM + 488
;        byte >MAP_CHAR_MEM + 496
;        byte >MAP_CHAR_MEM + 504
;        byte >MAP_CHAR_MEM + 512
;        byte >MAP_CHAR_MEM + 520
;        byte >MAP_CHAR_MEM + 528
;        byte >MAP_CHAR_MEM + 536
;        byte >MAP_CHAR_MEM + 544
;        byte >MAP_CHAR_MEM + 552
;        byte >MAP_CHAR_MEM + 560
;        byte >MAP_CHAR_MEM + 568
;        byte >MAP_CHAR_MEM + 576
;        byte >MAP_CHAR_MEM + 584
;        byte >MAP_CHAR_MEM + 592
;        byte >MAP_CHAR_MEM + 600
;        byte >MAP_CHAR_MEM + 608
;        byte >MAP_CHAR_MEM + 616
;        byte >MAP_CHAR_MEM + 624
;        byte >MAP_CHAR_MEM + 632        
;        byte >MAP_CHAR_MEM + 640
;        byte >MAP_CHAR_MEM + 648
;        byte >MAP_CHAR_MEM + 656
;        byte >MAP_CHAR_MEM + 664        
;        byte >MAP_CHAR_MEM + 672
;        byte >MAP_CHAR_MEM + 680
;        byte >MAP_CHAR_MEM + 688
;        byte >MAP_CHAR_MEM + 696
;        byte >MAP_CHAR_MEM + 704
;        byte >MAP_CHAR_MEM + 712
;        byte >MAP_CHAR_MEM + 720
;        byte >MAP_CHAR_MEM + 728
;        byte >MAP_CHAR_MEM + 736
;        byte >MAP_CHAR_MEM + 744
;        byte >MAP_CHAR_MEM + 752
;        byte >MAP_CHAR_MEM + 760
;        byte >MAP_CHAR_MEM + 768
;        byte >MAP_CHAR_MEM + 776
;        byte >MAP_CHAR_MEM + 784
;        byte >MAP_CHAR_MEM + 792
;        byte >MAP_CHAR_MEM + 800
;        byte >MAP_CHAR_MEM + 808
;        byte >MAP_CHAR_MEM + 816
;        byte >MAP_CHAR_MEM + 824
;        byte >MAP_CHAR_MEM + 832
;        byte >MAP_CHAR_MEM + 840
;        byte >MAP_CHAR_MEM + 848
;        byte >MAP_CHAR_MEM + 856
;        byte >MAP_CHAR_MEM + 864
;        byte >MAP_CHAR_MEM + 872
;        byte >MAP_CHAR_MEM + 880
;        byte >MAP_CHAR_MEM + 888
;        byte >MAP_CHAR_MEM + 896
;        byte >MAP_CHAR_MEM + 904
;        byte >MAP_CHAR_MEM + 912
;        byte >MAP_CHAR_MEM + 920
;        byte >MAP_CHAR_MEM + 928
;        byte >MAP_CHAR_MEM + 936
;        byte >MAP_CHAR_MEM + 944        
;        byte >MAP_CHAR_MEM + 952
;        byte >MAP_CHAR_MEM + 960
;        byte >MAP_CHAR_MEM + 968
;        byte >MAP_CHAR_MEM + 976
;        byte >MAP_CHAR_MEM + 984
;        byte >MAP_CHAR_MEM + 992
;        byte >MAP_CHAR_MEM + 1000
;        byte >MAP_CHAR_MEM + 1008
;        byte >MAP_CHAR_MEM + 1016        
;        byte >MAP_CHAR_MEM + 1024
;        byte >MAP_CHAR_MEM + 1032
;        byte >MAP_CHAR_MEM + 1040
;        byte >MAP_CHAR_MEM + 1048
;        byte >MAP_CHAR_MEM + 1056
;        byte >MAP_CHAR_MEM + 1064
;        byte >MAP_CHAR_MEM + 1072
;        byte >MAP_CHAR_MEM + 1080
;        byte >MAP_CHAR_MEM + 1088
;        byte >MAP_CHAR_MEM + 1096
;        byte >MAP_CHAR_MEM + 1104
;        byte >MAP_CHAR_MEM + 1112
;        byte >MAP_CHAR_MEM + 1120
;        byte >MAP_CHAR_MEM + 1128
;        byte >MAP_CHAR_MEM + 1136
;        byte >MAP_CHAR_MEM + 1144
;        byte >MAP_CHAR_MEM + 1152
;        byte >MAP_CHAR_MEM + 1160 
;        byte >MAP_CHAR_MEM + 1168
;        byte >MAP_CHAR_MEM + 1176
;        byte >MAP_CHAR_MEM + 1184
;        byte >MAP_CHAR_MEM + 1192
;        byte >MAP_CHAR_MEM + 1200
;        byte >MAP_CHAR_MEM + 1208
;        byte >MAP_CHAR_MEM + 1216
;        byte >MAP_CHAR_MEM + 1224
;        byte >MAP_CHAR_MEM + 1232
;        byte >MAP_CHAR_MEM + 1240
;        byte >MAP_CHAR_MEM + 1248
;        byte >MAP_CHAR_MEM + 1256
;        byte >MAP_CHAR_MEM + 1264
;        byte >MAP_CHAR_MEM + 1272
;        byte >MAP_CHAR_MEM + 1280
;        byte >MAP_CHAR_MEM + 1288
;        byte >MAP_CHAR_MEM + 1296
;        byte >MAP_CHAR_MEM + 1304
;        byte >MAP_CHAR_MEM + 1312
;        byte >MAP_CHAR_MEM + 1320
;        byte >MAP_CHAR_MEM + 1328
;        byte >MAP_CHAR_MEM + 1336
;        byte >MAP_CHAR_MEM + 1344
;        byte >MAP_CHAR_MEM + 1352       
;        byte >MAP_CHAR_MEM + 1360
;        byte >MAP_CHAR_MEM + 1368
;        byte >MAP_CHAR_MEM + 1376
;        byte >MAP_CHAR_MEM + 1384
;        byte >MAP_CHAR_MEM + 1392
;        byte >MAP_CHAR_MEM + 1400
;        byte >MAP_CHAR_MEM + 1408
;        byte >MAP_CHAR_MEM + 1416
;        byte >MAP_CHAR_MEM + 1424
;        byte >MAP_CHAR_MEM + 1432
;        byte >MAP_CHAR_MEM + 1440
;        byte >MAP_CHAR_MEM + 1448
;        byte >MAP_CHAR_MEM + 1456
;        byte >MAP_CHAR_MEM + 1464
;        byte >MAP_CHAR_MEM + 1472
;        byte >MAP_CHAR_MEM + 1480
;        byte >MAP_CHAR_MEM + 1488
;        byte >MAP_CHAR_MEM + 1496
;        byte >MAP_CHAR_MEM + 1504
;        byte >MAP_CHAR_MEM + 1512
;        byte >MAP_CHAR_MEM + 1520
;        byte >MAP_CHAR_MEM + 1528
;        byte >MAP_CHAR_MEM + 1536
;        byte >MAP_CHAR_MEM + 1544
;        byte >MAP_CHAR_MEM + 1552
;        byte >MAP_CHAR_MEM + 1560       
;        byte >MAP_CHAR_MEM + 1568
;        byte >MAP_CHAR_MEM + 1576
;        byte >MAP_CHAR_MEM + 1584
;        byte >MAP_CHAR_MEM + 1592
;        byte >MAP_CHAR_MEM + 1600
;        byte >MAP_CHAR_MEM + 1608
;        byte >MAP_CHAR_MEM + 1616
;        byte >MAP_CHAR_MEM + 1624
;        byte >MAP_CHAR_MEM + 1632
;        byte >MAP_CHAR_MEM + 1640
;        byte >MAP_CHAR_MEM + 1648
;        byte >MAP_CHAR_MEM + 1656
;        byte >MAP_CHAR_MEM + 1664
;        byte >MAP_CHAR_MEM + 1672
;        byte >MAP_CHAR_MEM + 1680
;        byte >MAP_CHAR_MEM + 1688
;        byte >MAP_CHAR_MEM + 1696
;        byte >MAP_CHAR_MEM + 1704
;        byte >MAP_CHAR_MEM + 1712
;        byte >MAP_CHAR_MEM + 1720
;        byte >MAP_CHAR_MEM + 1728
;        byte >MAP_CHAR_MEM + 1736
;        byte >MAP_CHAR_MEM + 1744
;        byte >MAP_CHAR_MEM + 1752
;        byte >MAP_CHAR_MEM + 1760
;        byte >MAP_CHAR_MEM + 1768
;        byte >MAP_CHAR_MEM + 1776
;        byte >MAP_CHAR_MEM + 1784
;        byte >MAP_CHAR_MEM + 1792
;        byte >MAP_CHAR_MEM + 1800
;        byte >MAP_CHAR_MEM + 1808
;        byte >MAP_CHAR_MEM + 1816
;        byte >MAP_CHAR_MEM + 1824
;        byte >MAP_CHAR_MEM + 1832
;        byte >MAP_CHAR_MEM + 1840
;        byte >MAP_CHAR_MEM + 1848
;        byte >MAP_CHAR_MEM + 1856
;        byte >MAP_CHAR_MEM + 1864
;        byte >MAP_CHAR_MEM + 1872
;        byte >MAP_CHAR_MEM + 1880       ;245

;MAPSCREEN2_CHSET_OFFSET_TABLE_LO
;        byte <MAP_CHAR_MEM
;        byte <MAP_CHAR_MEM + 100
;        byte <MAP_CHAR_MEM + 200
;        byte <MAP_CHAR_MEM + 300
;        byte <MAP_CHAR_MEM + 400
;        byte <MAP_CHAR_MEM + 500
;        byte <MAP_CHAR_MEM + 600
;        byte <MAP_CHAR_MEM + 700
;        byte <MAP_CHAR_MEM + 800
;        byte <MAP_CHAR_MEM + 900
;        byte <MAP_CHAR_MEM + 1000
;        byte <MAP_CHAR_MEM + 1100
;        byte <MAP_CHAR_MEM + 1200
;        byte <MAP_CHAR_MEM + 1300
;        byte <MAP_CHAR_MEM + 1400
;        byte <MAP_CHAR_MEM + 1500
;        byte <MAP_CHAR_MEM + 1600
;        byte <MAP_CHAR_MEM + 1700
;        byte <MAP_CHAR_MEM + 1800
;        byte <MAP_CHAR_MEM + 1900
;        byte <MAP_CHAR_MEM + 2000
;        byte <MAP_CHAR_MEM + 2100
;        byte <MAP_CHAR_MEM + 2200
;        byte <MAP_CHAR_MEM + 2300
;        byte <MAP_CHAR_MEM + 2400
;        byte <MAP_CHAR_MEM + 2500
;        byte <MAP_CHAR_MEM + 2600
;        byte <MAP_CHAR_MEM + 2700
;        byte <MAP_CHAR_MEM + 2800
;        byte <MAP_CHAR_MEM + 2900
;        byte <MAP_CHAR_MEM + 3000
;        byte <MAP_CHAR_MEM + 3100
;        byte <MAP_CHAR_MEM + 3200

;MAPSCREEN2_CHSET_OFFSET_TABLE_HI
;        byte >MAP_CHAR_MEM
;        byte >MAP_CHAR_MEM + 100
;        byte >MAP_CHAR_MEM + 200
;        byte >MAP_CHAR_MEM + 300
;        byte >MAP_CHAR_MEM + 400
;        byte >MAP_CHAR_MEM + 500
;        byte >MAP_CHAR_MEM + 600
;        byte >MAP_CHAR_MEM + 700
;        byte >MAP_CHAR_MEM + 800
;        byte >MAP_CHAR_MEM + 900
;        byte >MAP_CHAR_MEM + 1000
;        byte >MAP_CHAR_MEM + 1100
;        byte >MAP_CHAR_MEM + 1200
;        byte >MAP_CHAR_MEM + 1300
;        byte >MAP_CHAR_MEM + 1400
;        byte >MAP_CHAR_MEM + 1500
;        byte >MAP_CHAR_MEM + 1600
;        byte >MAP_CHAR_MEM + 1700
;        byte >MAP_CHAR_MEM + 1800
;        byte >MAP_CHAR_MEM + 1900
;        byte >MAP_CHAR_MEM + 2000
;        byte >MAP_CHAR_MEM + 2100
;        byte >MAP_CHAR_MEM + 2200
;        byte >MAP_CHAR_MEM + 2300
;        byte >MAP_CHAR_MEM + 2400
;        byte >MAP_CHAR_MEM + 2500
;        byte >MAP_CHAR_MEM + 2600
;        byte >MAP_CHAR_MEM + 2700
;        byte >MAP_CHAR_MEM + 2800
;        byte >MAP_CHAR_MEM + 2900
;        byte >MAP_CHAR_MEM + 3000
;        byte >MAP_CHAR_MEM + 3100
    
ATTRIB_ADDRESS
        word ATTRIBUTE_MEM

checkup
        byte %0000001
checkdown
        byte %0000010

checkleft
        byte %0000100

checkright
        byte %0001000

checkdownright
        byte %0001010

RIVER_ANIM1_LO       
        byte <CHRADR1
RIVER_ANIM1_HI
        byte >CHRADR1

RIVER_ANIM2_LO       
        byte <CHRADR2
RIVER_ANIM2_HI
        byte >CHRADR2

RIVER_ANIM3_LO       
        byte <CHRADR3
RIVER_ANIM3_HI
        byte >CHRADR3

RIVER_ANIM4_LO       
        byte <CHRADR4
RIVER_ANIM4_HI
        byte >CHRADR4

ZP1 word CHRADR1
ZP2 word CHRADR2
ZP3 word CHRADR3
ZP4 word CHRADR4

gamescore
        byte 0,0,0,0,0

animLevel
        byte 0

waterSpeed byte 0

maptwoloaded byte 0

; Chars.bin

;MAPS_CHAR_MEM

;;charset_data

;        byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;        byte $00,$FC,$FC,$FC,$FC,$FC,$FC,$00,$00,$7F,$7F,$7F,$7F,$7F,$7F,$00
;        byte $00,$1F,$1F,$1F,$1F,$1F,$1F,$00,$AA,$82,$92,$86,$92,$86,$82,$AA
;        byte $CF,$33,$FC,$33,$CF,$FC,$F3,$00,$08,$38,$38,$38,$28,$28,$08,$08
;        byte $00,$FE,$F8,$F8,$E0,$E0,$83,$83,$08,$08,$08,$08,$C8,$C8,$C8,$C8
;        byte $02,$CE,$38,$F8,$20,$E0,$83,$83,$C8,$C8,$C8,$C8,$C8,$C8,$C8,$C8
;        byte $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$BF,$BF,$BF,$BF,$BF,$BF,$BF,$BF
;        byte $FC,$FC,$FC,$FC,$FC,$FC,$FC,$00,$BF,$BF,$BF,$BF,$BF,$BF,$BF,$00
;        byte $FE,$FE,$F8,$F8,$E0,$E0,$80,$83,$02,$FE,$F8,$F8,$E0,$E0,$80,$80
;        byte $0F,$0F,$3F,$3F,$FF,$FF,$FF,$FF,$FE,$FE,$F8,$F8,$E0,$E0,$83,$83
;        byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$8F,$B3,$BC,$B3,$8F,$BC,$B3,$80
;        byte $8F,$B3,$BC,$B3,$8F,$BC,$B3,$00,$FE,$FE,$F8,$F8,$E0,$E0,$83,$83
;        byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$55,$AA,$55,$00,$00,$00,$00,$00
;        byte $AA,$02,$A8,$00,$20,$20,$80,$80,$02,$02,$08,$08,$20,$20,$80,$80
;        byte $02,$02,$08,$08,$20,$20,$8A,$8A,$00,$00,$00,$00,$00,$00,$AA,$AA
;        byte $00,$33,$FC,$30,$C2,$F2,$F8,$08,$00,$83,$BC,$33,$CF,$FC,$F3,$00
;        byte $A2,$AF,$A2,$80,$8F,$8F,$8F,$8F,$AA,$FF,$AA,$0A,$FE,$FF,$FF,$FF
;        byte $AA,$FE,$A8,$00,$23,$23,$8F,$8F,$80,$80,$8F,$8F,$8F,$8F,$8F,$8F
;        byte $02,$22,$08,$08,$23,$23,$8F,$8F,$00,$00,$3F,$3F,$3F,$3F,$3F,$3F
;        byte $3F,$3F,$FF,$FF,$00,$00,$AA,$AA,$3F,$3F,$3F,$3F,$00,$00,$AA,$AA
;        byte $CC,$33,$FC,$30,$F0,$C0,$C3,$00,$C8,$C8,$C8,$C8,$C8,$C8,$C8,$08
;        byte $0F,$03,$FC,$33,$CF,$FC,$F3,$00,$C8,$C8,$C8,$C8,$C8,$C8,$C8,$C8
;        byte $FF,$FE,$FE,$F8,$F8,$E0,$E3,$83,$8F,$0F,$3F,$3F,$FF,$FF,$FF,$FF
;        byte $8F,$0F,$3F,$FF,$FF,$FF,$FF,$FF,$55,$AA,$A5,$AF,$BF,$BF,$80,$80
;        byte $55,$AA,$55,$FF,$FF,$FF,$00,$00,$55,$AA,$5A,$FE,$FE,$FE,$02,$02
;        byte $BF,$80,$80,$80,$80,$80,$80,$AA,$FE,$C2,$32,$32,$0E,$0E,$02,$AA
;        byte $FF,$C3,$CC,$CC,$F0,$F0,$C0,$EA,$FE,$02,$02,$02,$02,$02,$02,$AA
;        byte $BF,$80,$80,$80,$80,$80,$AA,$BF,$FE,$02,$02,$02,$02,$02,$AA,$FE
;        byte $FF,$C0,$C0,$C0,$C0,$C0,$EA,$FF,$80,$80,$80,$90,$A5,$AA,$AA,$00
;        byte $03,$03,$02,$02,$02,$AA,$AA,$00,$C0,$C0,$40,$40,$40,$AA,$AA,$00
;        byte $02,$02,$02,$0A,$2A,$AA,$AA,$00,$55,$AA,$A5,$AF,$BF,$BF,$80,$80
;        byte $55,$AA,$55,$FF,$FF,$FF,$00,$00,$56,$AA,$6A,$FE,$FE,$FE,$02,$02
;        byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;        byte $02,$02,$02,$02,$02,$02,$02,$02,$00,$00,$00,$00,$00,$00,$AA,$AA
;        byte $00,$00,$00,$00,$00,$00,$AA,$AA,$02,$02,$02,$02,$02,$02,$AA,$AA
;        byte $02,$02,$02,$02,$02,$02,$02,$2A,$AA,$00,$00,$00,$00,$00,$00,$00
;        byte $AA,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$00,$00,$00,$00,$02,$02,$02,$03
;        byte $A0,$80,$80,$80,$00,$00,$00,$00,$2E,$2E,$2E,$2E,$2E,$2E,$2E,$2E
;        byte $08,$08,$20,$20,$20,$80,$AA,$AA,$2E,$2E,$2E,$2E,$2E,$2E,$AA,$AA
;        byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$00
;        byte $CA,$C8,$E3,$2F,$2F,$8F,$BF,$80,$AA,$2A,$2A,$02,$F2,$F2,$F2,$02
;        byte $CE,$32,$F2,$38,$CB,$CB,$E0,$E0,$0F,$33,$FC,$33,$CF,$FC,$F3,$00
;        byte $C2,$32,$F2,$F2,$32,$C2,$F2,$02,$CF,$33,$FC,$FC,$32,$C2,$F2,$08
;        byte $C8,$C8,$E0,$2C,$20,$8C,$B0,$80,$CA,$C8,$E0,$23,$2F,$8C,$B3,$80
;        byte $AA,$2A,$EA,$32,$C2,$F2,$F2,$02,$CE,$32,$F2,$38,$CB,$C8,$E3,$20
;        byte $C2,$32,$F2,$32,$C2,$F2,$F2,$02,$00,$00,$00,$00,$02,$02,$02,$08
;        byte $2F,$8F,$BF,$BF,$BF,$BF,$BF,$BF,$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2
;        byte $08,$08,$20,$2C,$2C,$8C,$BC,$BC,$F2,$F2,$F8,$C8,$CB,$E3,$23,$2F
;        byte $FF,$FC,$FC,$FE,$F2,$F2,$F8,$C8,$8F,$8F,$BF,$3F,$3F,$FF,$FF,$FF
;        byte $FF,$FF,$FF,$FF,$FF,$FC,$FC,$FE,$C8,$E3,$23,$2F,$8F,$8F,$BF,$3F
;        byte $3F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$33,$FC,$33,$CF,$FC,$F3,$00
;        byte $00,$C8,$C8,$C8,$C8,$C8,$C8,$08,$AA,$FF,$AA,$00,$FF,$FF,$FF,$FF
;        byte $88,$C8,$88,$08,$C8,$C8,$C8,$C8,$00,$00,$FC,$FC,$FC,$FC,$FC,$FC
;        byte $00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$08,$08,$C8,$C8,$C8,$C8,$C8,$C8
;        byte $FC,$FC,$FC,$FC,$00,$00,$A8,$A8,$FF,$FF,$FF,$FF,$00,$00,$AA,$AA
;        byte $3F,$3F,$3F,$3F,$00,$00,$2A,$2A,$C8,$C8,$C8,$C8,$08,$08,$80,$80
;        byte $00,$33,$FC,$33,$C2,$FA,$F9,$09,$00,$33,$FC,$33,$AA,$55,$55,$00
;        byte $AA,$FF,$AA,$00,$00,$00,$00,$00,$25,$24,$27,$27,$27,$24,$80,$83
;        byte $00,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$FC,$FF,$FF,$FF,$FF,$FC,$00
;        byte $AA,$BB,$BF,$B8,$B8,$3B,$3B,$38,$FF,$C0,$2A,$3F,$FF,$3F,$EA,$00
;        byte $FF,$00,$AA,$FF,$FF,$FF,$AA,$00,$38,$3B,$3B,$3B,$3B,$38,$3A,$00
;        byte $00,$FF,$FF,$FF,$FF,$00,$AA,$00,$00,$CC,$3F,$CC,$AA,$55,$55,$00
;        byte $00,$CC,$3F,$CC,$83,$AF,$6F,$60,$00,$CC,$3F,$CC,$F3,$3F,$CF,$00
;        byte $00,$3F,$FF,$FF,$FF,$FF,$3F,$00,$58,$18,$D8,$D8,$D8,$18,$42,$02
;        byte $FF,$03,$A8,$FC,$FF,$FC,$AB,$00,$AA,$EE,$EE,$2E,$2A,$EC,$EC,$2C
;        byte $2C,$EC,$EC,$EC,$EC,$2C,$AC,$00,$CF,$33,$FC,$33,$C2,$FA,$F9,$09
;        byte $CF,$33,$FC,$33,$AA,$55,$55,$00,$25,$24,$27,$27,$27,$24,$81,$80
;        byte $00,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$FC,$FF,$FF,$FF,$FF,$FC,$00
;        byte $AA,$BB,$BB,$B8,$AA,$3B,$3B,$38,$FF,$C0,$2A,$BF,$FF,$BF,$EA,$00
;        byte $FF,$00,$AA,$FF,$FF,$FF,$AA,$00,$38,$3B,$3B,$3B,$3B,$38,$3A,$00
;        byte $00,$FF,$FF,$FF,$FF,$00,$AA,$00,$CF,$33,$FC,$33,$A3,$6B,$6B,$18
;        byte $00,$0F,$3F,$3F,$3F,$3F,$0F,$00,$08,$C8,$F8,$F8,$F8,$F8,$CE,$02
;        byte $FF,$00,$A8,$FE,$FF,$FE,$A8,$00,$A8,$B8,$B8,$B8,$A8,$20,$2C,$2C
;        byte $2C,$EC,$EC,$EC,$EC,$2C,$A0,$00,$00,$33,$F0,$C3,$CF,$CC,$33,$00
;        byte $0A,$0A,$02,$08,$02,$08,$02,$0A,$AF,$AF,$AF,$AF,$AF,$AF,$AF,$AF
;        byte $A5,$A5,$A5,$A5,$A5,$A5,$A5,$A5,$00,$00,$00,$00,$00,$00,$00,$00
;        byte $0A,$0A,$02,$08,$02,$08,$02,$00,$AF,$AF,$AF,$AF,$AF,$AF,$AF,$00
;        byte $A5,$A5,$A5,$A5,$A5,$A5,$A5,$00,$0F,$33,$3C,$33,$0F,$3C,$33,$00
;        byte $A5,$A5,$A5,$A5,$A5,$A5,$A5,$A5,$0A,$0A,$02,$08,$02,$08,$00,$00
;        byte $AF,$AF,$AF,$AF,$AF,$AF,$00,$00,$A5,$A5,$A5,$A5,$A5,$A5,$00,$00
;        byte $00,$00,$00,$00,$00,$03,$0F,$3F,$00,$00,$00,$00,$00,$00,$00,$00
;        byte $15,$00,$0A,$0A,$20,$23,$23,$00,$00,$08,$08,$08,$20,$23,$23,$00
;        byte $55,$FF,$54,$00,$00,$00,$03,$03,$0C,$0C,$30,$30,$C0,$C0,$00,$AA
;        byte $00,$00,$00,$00,$00,$00,$FF,$AA,$55,$AA,$AA,$AA,$AA,$AA,$AA,$00
;        byte $55,$AA,$A0,$83,$8F,$8C,$8C,$00,$55,$AA,$02,$F0,$FC,$0C,$0C,$00
;        byte $00,$00,$00,$FF,$FF,$FF,$00,$00,$AF,$FF,$EB,$FF,$AF,$FF,$FF,$FF
;        byte $AF,$FF,$EB,$FF,$AF,$FF,$EB,$FF,$AF,$FF,$EB,$FF,$AF,$FF,$00,$00
;        byte $CC,$30,$FC,$30,$CC,$FC,$F0,$00,$25,$20,$23,$23,$23,$20,$2A,$00
;        byte $55,$02,$F2,$32,$F2,$02,$AA,$00,$AC,$FC,$E8,$FC,$AC,$FC,$E8,$FC
;        byte $AC,$FC,$E8,$FC,$AC,$FC,$00,$00,$7C,$7C,$7C,$7C,$7C,$7C,$7C,$00
;        byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$00,$55,$AA,$80,$82,$88,$88,$88,$88
;        byte $55,$AA,$00,$AA,$00,$FF,$FF,$FF,$55,$AA,$02,$82,$22,$22,$22,$22
;        byte $88,$88,$88,$88,$82,$80,$AA,$00,$FF,$FF,$FF,$00,$AA,$00,$AA,$00
;        byte $22,$22,$22,$22,$82,$02,$AA,$00,$FD,$FD,$FD,$FD,$FD,$FD,$FD,$00
;        byte $3D,$3D,$3D,$3D,$3D,$3D,$3D,$00,$F3,$CC,$3F,$CC,$F3,$3F,$CF,$00
;        byte $15,$2A,$2F,$2C,$2F,$2F,$2A,$00,$54,$A8,$F8,$F8,$38,$F8,$A8,$00
;        byte $54,$08,$C8,$08,$C8,$08,$A8,$00,$55,$AA,$AA,$AA,$AA,$95,$90,$90
;        byte $55,$AA,$AA,$AA,$AA,$55,$01,$01,$54,$A8,$A8,$A8,$A8,$A8,$A8,$A8
;        byte $90,$90,$95,$9F,$9F,$95,$AA,$00,$01,$01,$55,$7D,$7D,$55,$AA,$00
;        byte $A8,$A8,$A8,$A8,$A8,$A8,$A8,$00,$54,$A8,$A8,$A8,$A8,$A8,$A8,$00
;        byte $00,$00,$00,$F0,$F0,$F0,$00,$00,$A3,$F3,$E3,$F3,$A3,$F3,$E3,$F3
;        byte $A0,$F0,$E0,$F0,$A0,$F0,$E0,$F0,$A3,$F3,$E3,$F3,$A3,$F3,$00,$00
;        byte $A0,$F0,$E0,$F0,$A0,$F0,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00
;        byte $00,$00,$00,$00,$03,$0F,$3F,$FF,$03,$0F,$3F,$FF,$FF,$FF,$FF,$FF
;        byte $55,$00,$C8,$08,$20,$20,$20,$00,$55,$00,$CA,$0A,$A0,$A3,$A3,$00
;        byte $00,$CA,$CA,$0A,$A0,$A3,$A3,$00,$00,$C8,$C8,$08,$20,$23,$23,$00
;        byte $00,$88,$88,$08,$20,$20,$23,$03,$00,$4A,$4A,$4A,$C0,$C2,$F2,$30
;        byte $00,$C8,$C8,$08,$20,$20,$20,$00,$00,$00,$00,$03,$03,$03,$03,$03
;        byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$3F,$3F
;        byte $03,$03,$03,$03,$03,$57,$FF,$FF,$30,$30,$30,$30,$30,$75,$FF,$FF
;        byte $03,$03,$03,$03,$03,$57,$FF,$FF,$00,$00,$00,$00,$00,$55,$FF,$FF
;        byte $00,$00,$00,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$D5,$FF,$FF
;        byte $00,$00,$00,$00,$00,$50,$FC,$FC,$C0,$C0,$C0,$C0,$C0,$D5,$FF,$FF
;        byte $0C,$0C,$0C,$0C,$0C,$5D,$FF,$FF,$03,$03,$03,$03,$03,$03,$03,$03
;        byte $80,$80,$80,$80,$80,$80,$80,$80,$AA,$FF,$AA,$00,$00,$40,$41,$75
;        byte $FF,$FF,$FF,$32,$0A,$02,$02,$02,$FF,$FF,$FF,$8C,$A0,$40,$40,$40
;        byte $02,$02,$02,$02,$02,$02,$AF,$BF,$40,$40,$40,$40,$40,$40,$F2,$FE
;        byte $00,$00,$00,$00,$00,$40,$41,$45,$00,$00,$00,$00,$00,$00,$50,$54
;        byte $3F,$3F,$0F,$00,$00,$00,$00,$00,$FF,$FF,$FF,$32,$0A,$02,$02,$02
;        byte $FF,$FF,$FF,$8C,$A0,$40,$40,$40,$FC,$FC,$F0,$00,$00,$00,$00,$00
;        byte $02,$02,$02,$02,$02,$02,$AF,$BF,$40,$40,$40,$40,$40,$40,$FA,$FE
;        byte $C0,$30,$F0,$30,$C0,$F0,$F0,$00,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$00
;        byte $FE,$3E,$02,$02,$02,$02,$02,$02,$FC,$F0,$00,$00,$00,$00,$00,$00
;        byte $02,$02,$02,$02,$0A,$2A,$AA,$55,$00,$00,$00,$00,$80,$A0,$A8,$54
;        byte $FC,$FC,$FC,$FC,$10,$00,$00,$00

;; CharsAttribs.bin
;;color_data
;ATTRIBUTES_MEM

;        byte $03,$0A,$0A,$0A,$0A,$3E,$0A,$0A,$0A,$0A,$0A,$0A,$0E,$0E,$0E,$0E
;        byte $0E,$0E,$0E,$0E,$1E,$0A,$0A,$0A,$0E,$0B,$0A,$0A,$0A,$0A,$0A,$0A
;        byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0E,$0E,$0E,$0E,$1C
;        byte $1C,$1C,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$08,$0C,$0C,$08,$0C,$0C,$0C
;        byte $0B,$0E,$0E,$0E,$0C,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0A,$0A
;        byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0E,$0E,$0E,$0E,$0E
;        byte $0E,$0E,$0E,$0E,$0E,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A
;        byte $0A,$0A,$0A,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0A,$0A,$0A,$0D,$0D
;        byte $0D,$0D,$0D,$0A,$0A,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0A,$0C,$0C
;        byte $0C,$0C,$0C,$0A,$0E,$09,$0E,$09,$0E,$09,$0E,$0A,$09,$0E,$09,$0E
;        byte $0E,$0C,$0C,$0C,$0E,$0E,$0E,$0A,$0E,$0E,$0A,$0A,$0A,$0A,$0A,$0E
;        byte $0E,$0A,$0A,$0A,$0A,$0A,$0B,$0A,$0A,$0B,$0A,$0A,$0A,$0A,$0F,$0F
;        byte $0E,$0A,$0A,$0A,$0A,$0D,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$1E,$0E,$0E
;        byte $0C,$0C,$0C,$0C,$0B,$0B,$0C,$0A,$0D,$8A,$8A,$8E,$8E,$0E,$8A,$0A
;        byte $0A,$0E,$0E,$0C,$0C,$0A,$0E,$0E,$0E,$0E,$0C,$0C,$0A,$0A,$0A,$0A
;        byte $0A,$0A,$0A,$0A,$0E,$0C,$0B,$0B,$0B

;; Tiles.bin
;;tiles_data
;TILES_MEM

;        byte $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;        byte $02,$03,$04,$05,$06,$06,$05,$07,$06,$05,$08,$09,$05,$0A,$06,$0B
;        byte $0C,$0D,$0E,$05,$0C,$0F,$05,$10,$0E,$05,$11,$12,$05,$13,$12,$14
;        byte $06,$15,$06,$05,$06,$16,$05,$17,$0E,$05,$11,$12,$05,$13,$12,$18
;        byte $19,$19,$19,$05,$01,$01,$05,$1A,$01,$05,$1B,$01,$05,$1C,$1D,$1D
;        byte $1E,$1F,$06,$05,$20,$21,$05,$22,$23,$05,$24,$25,$05,$1C,$26,$27
;        byte $28,$06,$06,$29,$2A,$06,$06,$29,$18,$18,$18,$2B,$18,$18,$18,$2B
;        byte $2C,$2D,$18,$18,$2E,$18,$18,$18,$14,$14,$14,$14,$14,$14,$14,$14
;        byte $2F,$30,$30,$31,$32,$33,$34,$35,$36,$37,$38,$37,$39,$3A,$3B,$3C
;        byte $3D,$3E,$3E,$3F,$40,$41,$41,$42,$41,$41,$40,$42,$43,$43,$44,$45
;        byte $3D,$3E,$3E,$3F,$40,$46,$47,$48,$49,$4A,$40,$4B,$4C,$43,$44,$4D
;        byte $4E,$4F,$50,$51,$06,$52,$53,$54,$55,$16,$06,$54,$56,$15,$06,$54
;        byte $06,$06,$57,$58,$06,$59,$06,$5A,$5B,$5C,$18,$5D,$5E,$0D,$18,$5D
;        byte $18,$18,$18,$5F,$18,$18,$60,$61,$14,$62,$63,$18,$14,$5F,$64,$18
;        byte $18,$18,$18,$2B,$18,$18,$18,$2B,$14,$14,$14,$2B,$14,$14,$14,$2B
;        byte $65,$65,$65,$66,$67,$67,$67,$68,$69,$6A,$25,$6B,$6C,$6D,$6E,$6F
;        byte $65,$70,$71,$71,$72,$73,$74,$75,$76,$77,$78,$78,$79,$7A,$7A,$7A
;        byte $7B,$7B,$7C,$7D,$7E,$74,$7F,$72,$78,$78,$80,$81,$7A,$7A,$7A,$82
;        byte $06,$83,$84,$84,$06,$85,$86,$87,$88,$89,$8A,$8A,$8B,$8C,$8C,$8C
;        byte $84,$84,$8D,$06,$8E,$86,$8F,$06,$8A,$8A,$90,$91,$8C,$8C,$8C,$92
;        byte $93,$65,$65,$65,$67,$67,$67,$67,$6A,$25,$6A,$25,$6D,$6E,$6D,$6E
;        byte $94,$95,$96,$97,$98,$99,$9A,$97,$94,$95,$96,$40,$98,$99,$9A,$40
;        byte $94,$95,$96,$9B,$98,$99,$9A,$9B,$94,$95,$9C,$40,$9D,$9E,$9F,$A0
;        byte $94,$95,$96,$9B,$98,$99,$9A,$9B,$94,$95,$96,$9B,$98,$99,$9A,$9B
;        byte $94,$95,$96,$A1,$98,$99,$9A,$A1,$94,$95,$96,$A1,$98,$99,$9A,$40
;        byte $94,$95,$96,$A2,$98,$99,$9A,$A3,$94,$95,$96,$A2,$98,$99,$9A,$A3
;        byte $94,$95,$96,$A4,$98,$99,$9A,$A5,$94,$95,$96,$A6,$98,$99,$9A,$40
;        byte $A7,$A8,$A9,$A7,$AA,$AA,$AA,$AA,$AB,$AB,$AC,$AC,$AD,$AD,$AD,$AD
;        byte $06,$AE,$AF,$B0,$06,$AA,$AA,$AA,$0C,$B1,$AC,$AC,$0C,$B2,$AD,$AD
;        byte $B3,$B3,$B3,$B4,$06,$06,$06,$06,$B5,$B6,$B6,$B7,$B8,$B9,$B9,$BA
;        byte $BB,$BC,$BC,$BC,$BD,$BD,$BD,$BD,$BD,$BD,$BE,$BF,$BD,$BD,$AF,$C0
;        byte $B3,$B3,$B3,$B4,$06,$06,$06,$06,$C1,$C2,$C3,$06,$C4,$C5,$C6,$06
;        byte $AF,$B0,$C7,$06,$AA,$AA,$AA,$C8,$AC,$AC,$C9,$CA,$AD,$AD,$CB,$CC
;        byte $18,$18,$18,$18,$14,$14,$14,$14,$14,$14,$14,$14,$CD,$CD,$CD,$CD
;        byte $18,$18,$18,$18,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14
;        byte $06,$06,$06,$06,$06,$06,$06,$06,$40,$CE,$CF,$18,$CF,$18,$18,$18
;        byte $06,$06,$06,$06,$06,$06,$06,$06,$18,$18,$18,$18,$18,$18,$18,$18
;        byte $D0,$D1,$D0,$D1,$D2,$D3,$D2,$D3,$D3,$D2,$D3,$D2,$D2,$D3,$D2,$D3
;        byte $D0,$D1,$D0,$D1,$D2,$D4,$D5,$D6,$D6,$D2,$D6,$D2,$D6,$D2,$D6,$D2
;        byte $A1,$A1,$A1,$A1,$40,$D7,$40,$D8,$D9,$DA,$97,$97,$DB,$DC,$DD,$DD
;        byte $A1,$A1,$A1,$A1,$D8,$40,$DE,$40,$97,$97,$DF,$E0,$DD,$DD,$E1,$E2
;        byte $A1,$E3,$E4,$A1,$D8,$E3,$E4,$40,$97,$E3,$E4,$97,$D8,$E3,$E4,$A1
;        byte $65,$65,$65,$65,$72,$E5,$72,$72,$40,$E6,$E7,$40,$44,$E8,$E9,$44
;        byte $A1,$A1,$A1,$A1,$01,$EA,$EB,$40,$EC,$ED,$EE,$EF,$44,$F0,$F1,$44
;        byte $06,$06,$06,$F2,$06,$06,$06,$F2,$06,$06,$06,$F2,$06,$06,$06,$F2
;        byte $F3,$B4,$4F,$4F,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
;        byte $00,$F4,$F5,$00,$40,$F6,$F7,$40,$00,$F8,$F8,$40,$00,$00,$00,$00
;        byte $19,$19,$19,$19,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;; Map.bin
;;map_data
;MAPS_MEM

;        byte $00,$2E,$15,$29,$00,$2E,$29,$00,$2E,$00,$29,$00,$2E,$29,$15,$29
;        byte $26,$25,$19,$29,$25,$26,$25,$25,$26,$25,$29,$25,$26,$29,$15,$29
;        byte $28,$2B,$18,$29,$00,$00,$00,$28,$2B,$27,$29,$28,$2B,$29,$15,$29
;        byte $08,$08,$1A,$08,$08,$2F,$04,$08,$08,$08,$08,$08,$08,$08,$1A,$08
;        byte $2D,$2D,$17,$2D,$0B,$01,$2D,$1E,$1D,$1F,$2D,$2D,$2D,$2D,$17,$2D
;        byte $24,$24,$16,$0C,$03,$06,$24,$1C,$1B,$20,$23,$12,$13,$23,$16,$23
;        byte $22,$22,$0D,$02,$07,$0E,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22
;        byte $14,$14,$05,$14,$14,$0F,$0F,$2A,$10,$11,$2A,$0F,$14,$14,$14,$14
;        byte $21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21
;        byte $21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21

