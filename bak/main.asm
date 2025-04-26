 ;===============================================================================
; Commodore 64: "Hotel Map"
;
; File: Project 12-13: "Enemy Attacks"
;===============================================================================
 
; Bugs found: When scrolling the screen, sometimes the wall detection is not found
; and sprite passes through it.

; Sprite animation looping is off. The animation is running fast
; Sprite Idle animation is not showing.
;===============================================================================
; SCROLLING MAP EXAMPLE 1 - C64 YouTube Game Project
; 2016/17 - Peter 'Sig' Hewett aka RetroRomIcon (contributions)
; Additional coding by Steve Morrow
;===============================================================================
Operator Calc        ; IMPORTANT - calculations are made BEFORE hi/lo bytes
                     ;             in precidence (for expressions and tables)
;===============================================================================
; DEFINITIONS
;===============================================================================
IncAsm "VIC_Registers.asm"             ; VICII register includes
IncAsm "Game_Macros.asm                    ; macro includes
;===============================================================================
;===============================================================================
; CONSTANTS
;===============================================================================

CONSOLE_TEXT = SPRITE_CONSOLE_TEXT
CONSOLE_DISPLAY = DisplaySpriteInfo

#region "Constants"
SCREEN1_MEM  = $4000                 ; Bank 1 - Screen 0 ; $4000
SCREEN2_MEM  = $4400                 ; Bank 1 - Screen 1 ; $4400
SCORE_SCREEN = $5800                 ; Bank 1 - Screen 6 ; $5800

;MAP_MEM = $8000
;ATTRIBUTE_MEM = MAP_MEM + 2
;TILE_MEM = ATTRIBUTE_MEM + 2


; ****** CURRENT PROJECT *******
; Start $4800: 18432

; Top blue background: 18680
; Rock stacked wall: 19112
; Regular brick wall: 19256
; Pole facing to left: 19304
; Top part of wall (behind window): 19544
; Solid water part: 19744, 19888
; Rock stacked wall: 19864

CHRADR1 = 19992                      ; middle of rope
CHRADR2 = 19144                      ; top of window
CHRADR3 = 20128                      ; top water tile: 20288
CHRADR3 = 19544                     ; 20200, 20080: rock stacked wall

; 19544 = top part of orange (background wall)
; 20128 = top middle of rope facing right
; 20144 = top part of rope facing right
; 20192 = center of rope facing right
; 20224 = top/center of rope facing left (top part)

; 20320 = top of water
CHRADR4 = 20312

COLOR_MEM  = $D800                   ; Color mem never changes

SPRITE_POINTER_BASE = SCREEN1_MEM + $3f8 ; last 8 bytes of screen mem

SPRITE_BASE = $70                       ; the pointer to the first image#

SPRITE_0_PTR = SPRITE_POINTER_BASE + 0  ; Sprite pointers
SPRITE_1_PTR = SPRITE_POINTER_BASE + 1
SPRITE_2_PTR = SPRITE_POINTER_BASE + 2
SPRITE_3_PTR = SPRITE_POINTER_BASE + 3
SPRITE_4_PTR = SPRITE_POINTER_BASE + 4
SPRITE_5_PTR = SPRITE_POINTER_BASE + 5
SPRITE_6_PTR = SPRITE_POINTER_BASE + 6
SPRITE_7_PTR = SPRITE_POINTER_BASE + 7

; Set map walking limits
MAP_RIGHT_LIMIT = 17
MAP_DOWN_LIMIT = 10

; FarmLife
TILE_DEPTH1 = 16
TILE_DEPTH2 = 32
TILE_DEPTH3 = 48
TILE_DEPTH4 = 64
TILE_DEPTH5 = 80
TILE_DEPTH6 = 96
TILE_DEPTH7 = 112
TILE_DEPTH8 = 128
TILE_DEPTH9 = 144
TILE_DEPTH10 = 160
TILE_DEPTH11 = 176
TILE_DEPTH12 = 192
TILE_DEPTH13 = 208
TILE_DEPTH14 = 224
TILE_DEPTH15 = 240
TILE_DEPTH16 = 256
TILE_DEPTH17 = 272
TILE_DEPTH18 = 288
TILE_DEPTH19 = 304
TILE_DEPTH20 = 320
TILE_DEPTH21 = 336

TILE2_DEPTH1 = 17
TILE2_DEPTH2 = 34
TILE2_DEPTH3 = 51
TILE2_DEPTH4 = 68
TILE2_DEPTH5 = 85
TILE2_DEPTH6 = 102
TILE2_DEPTH7 = 119
TILE2_DEPTH8 = 136
TILE2_DEPTH9 = 153
TILE2_DEPTH10 = 170
TILE2_DEPTH11 = 187
TILE2_DEPTH12 = 204
TILE2_DEPTH13 = 221
TILE2_DEPTH14 = 238
TILE2_DEPTH15 = 255
TILE2_DEPTH16 = 272
TILE2_DEPTH17 = 289
TILE2_DEPTH18 = 306
TILE2_DEPTH19 = 323
TILE2_DEPTH20 = 340
TILE2_DEPTH21 = 357

SPRITE_DELTA_OFFSET_X = 8               ; Offset from SPRITE coords to Delta Char coords
SPRITE_DELTA_OFFSET_Y = 14

ENEMY_SPRITE_DELTA_OFFSET_X = 8
ENEMY_SPRITE_DELTA_OFFSET_Y = 14

NUMBER_OF_SPRITES_DIV_4 = 3           ; This is for my personal version, which
                                      ; loads sprites and characters under IO ROM
LEVEL_1_MAP   = $E000                 ;Address of level 1 tiles/charsets
#endregion

;===============================================================================
; GAME TIMERS
;===============================================================================
CIA1_TIMA_LO = $dc04
CIA1_TIMA_HI = $dc05
CIA1_TIMB_LO = $dc06
CIA1_TIMB_HI = $dc07

;===============================================================================
; ZERO PAGE LABELS
;===============================================================================
#region "ZeroPage"
PARAM1 = $03                 ; These will be used to pass parameters to routines
PARAM2 = $04                 ; when you can't use registers or other reasons
PARAM3 = $05                            
PARAM4 = $06                 ; essentially, think of these as extra data registers

PARAM5 = $07

TIMER = $08                  ; Timers - fast and slow, updated every frame
SLOW_TIMER = $09

WPARAM1 = $0A                ; Word length Params. Same as above only room for 2
WPARAM2 = $0C                ; bytes (or an address)
WPARAM3 = $0E

;---------------------------- $11 - $16 available

ZEROPAGE_POINTER_1 = $17
ZEROPAGE_POINTER_2 = $19
ZEROPAGE_POINTER_3 = $21
ZEROPAGE_POINTER_4 = $23

CURRENT_SCREEN   = $25       ; Pointer to current front screen
CURRENT_BUFFER   = $27       ; Pointer to current back buffer

SCROLL_COUNT_X   = $29       ; Current hardware scroll value
SCROLL_COUNT_Y   = $2A
SCROLL_SPEED     = $2B       ; Scroll speed (not implemented yet)
SCROLL_DIRECTION = $2C       ; Direction we are scrolling in
SCROLL_MOVING    = $2D       ; are we moving? (Set to direction of scrolling)
                             ; This is for resetting back to start frames

                            ; All data is for the top left corner of the visible map area
MAP_POS_ADDRESS = $2E       ; (2 bytes) pointer to current address in the level map
MAP_X_POS       = $30       ; Current map x position (in tiles)
MAP_Y_POS       = $31       ; Current map y position (in tiles)
MAP_X_DELTA     = $32       ; Map sub tile delta (in characters)
MAP_Y_DELTA     = $33       ; Map sub tile delta (in characters)

ENMAP_X_POS       = $34       ; Current map x position (in tiles)
ENMAP_Y_POS       = $35       ; Current map y position (in tiles)
ENMAP_X_DELTA     = $36       ; Map sub tile delta (in characters)
ENMAP_Y_DELTA     = $37       ; Map sub tile delta (in characters)

#endregion

;===============================================================================
; BASIC KICKSTART
;===============================================================================
KICKSTART
; Sys call to start the program - 10 SYS (2064)

;*=$0801

;        BYTE $0E,$08,$0A,$00,$9E,$20,$28,$32,$30,$36,$34,$29,$00,$00,$00

;===============================================================================
; START OF GAME PROJECT
;===============================================================================
*=$0810

PRG_START
        lda #0                          ; Turn off sprites 
        sta VIC_SPRITE_ENABLE

        lda VIC_SCREEN_CONTROL          ; turn screen off with bit 4
        and #%11100000                  ; mask out bit 4 - Screen on/off
        sta VIC_SCREEN_CONTROL          ; save back - setting bit 4 to off

;===============================================================================
; SETUP VIC BANK MEMORY
;===============================================================================
#region "VIC Setup"
        ; To set the VIC bank we have to change the first 2 bits in the
        ; CIA 2 register. So we want to be careful and only change the
        ; bits we need to.

        lda VIC_BANK            ; Fetch the status of CIA 2 ($DD00)
        and #%11111100          ; mask for bits 2-8
        ora #%00000010          ; the first 2 bits are your desired VIC bank value
                                ; In this case bank 1 ($4000 - $7FFF)
        sta VIC_BANK
       

;===============================================================================
; CHARACTER SET ENABLE: SCREEN MEMORY
;===============================================================================
        ; Within the VIC Bank we can set where we want our screen and character
        ; set memory to be using the VIC_MEMORY_CONTROL at $D018
        ; It is important to note that the values given are RELATIVE to the start
        ; address of the VIC bank you are using.
       
        lda #%00000010   ; bits 1-3 (001) = character memory 2 : $0800 - $0FFF
                         ; bits 4-7 (000) = screen memory 0 : $0000 - $03FF

        sta VIC_MEMORY_CONTROL

        ; Because these are RELATIVE to the VIC banks base address (Bank 1 = $4000)
        ; this gives us a base screen memory address of $4000 and a base
        ; character set memory of $4800
        ; 
        ; Sprite pointers are the last 8 bytes of screen memory (25 * 40 = 1000 and
        ; yet each screen reserves 1024 bytes). So Sprite pointers start at
        ; $4000 + $3f8.

        ; After alloction of VIC Memory for Screen, backbuffer, scoreboard, and
        ; 2 character sets , arranged to one solid block of mem,
        ; Sprite data starts at $5C00 - giving the initial image a pointer value of $70
        ; and allowing for up to 144 sprite images

#endregion  
;===============================================================================
; SYSTEM INITIALIZATION
;===============================================================================
#region "System Setup"
System_Setup

        ; Here is where we copy level 1 data from the start setup to under
        ; $E000 so we can use it later when the game resets.
        ; A little bank switching is involved here.
        sei           

        ; Here you load and store the Processor Port ($0001), then use 
        ; it to turn off LORAM (BASIC), HIRAM (KERNAL), CHAREN (CHARACTER ROM)
        ; then use a routine to copy your sprite and character mem under there
        ; before restoring the original value of $0001 and turning interrupts
        ; back on.

        lda PROC_PORT                   ; store ram setup
        sta PARAM1

        lda #%00110000                  ; Switch out BASIC, KERNAL, CHAREN, IO
        sta PROC_PORT

        ; When the game starts, Level 1 tiles and characters are stored in place to run,
        ; However, when the game resets we will need to restore these levels intact.
        ; So we're saving them away to load later under the KERNAL at $E000-$EFFF (4k)
        ; To do this we need to do some bank switching, copy data, then restore as
        ; we may use the KERNAL later for some things.

        lda PARAM1                      ; restore ram setup
        sta PROC_PORT
        cli
#endregion
;===============================================================================
; SCREEN SETUP
;===============================================================================
#region "Screen Setup"
Screen_Setup
        lda #COLOR_BLACK
        sta VIC_BACKGROUND_COLOR 
        lda #COLOR_GREY2
        sta VIC_CHARSET_MULTICOLOR_1
        lda #COLOR_GREY1
        sta VIC_CHARSET_MULTICOLOR_2

        loadPointer CURRENT_SCREEN,SCREEN1_MEM
        loadPointer CURRENT_BUFFER,SCREEN2_MEM

; Load in Hotel: Chars, Map, Attribs & Tiles
; from our Disk Image

        ldx #0
        jsr LoadChsetFromDisk
        ;ldx #1
        ;jsr LoadChsetFromDisk
        ;ldx #2
        ;jsr LoadChsetFromDisk
        ;ldx #3
        ;jsr LoadChsetFromDisk
        
        ;jsr LoadHotelTilesFromDisk

        ;jsr LoadHotelMap
        ;jsr LoadNewTiles

        ldx #3                        ; (129,26=default), 61
        ldy #2                          ; , 27

        jsr DrawMap                     ; Draw the level map (Screen1)
                                        ; And initialize it

        jsr CopyToBuffer                ; Copy to the backbuffer(Screen2)


;*******************************************************************************
;               TURN OFF 2nd Screen Stats Display (FOR NOW)
;******************************************************************************'

        ;loadpointer ZEROPAGE_POINTER_1, CONSOLE_TEXT
        loadpointer ZEROPAGE_POINTER_1, ADMIN_TEST_DISPLAY      ; Debugger
        ;loadpointer ZEROPAGE_POINTER_1, GAME_PANEL_DISPLAY

        lda #0                          ; PARAM1 contains X screen coord (column)
        sta PARAM1
        lda #19                         ; PARAM2 contains Y screen coord (row)
        sta PARAM2
        lda #COLOR_WHITE                ; PARAM3 contains the color to use
        sta PARAM3
        jsr DisplayText                 ; Then we display the stats panel
       
        jsr WaitFrame
        jsr InitRasterIRQ               ; Setup raster interrupts
        jsr WaitFrame

        
        lda #%00011011                  ; Default (Y scroll = 3 by default)    
        sta VIC_SCREEN_CONTROL
        lda #COLOR_BLACK
        sta VIC_BORDER_COLOR

#endregion

;===============================================================================
;  SPRITE SETUP
;===============================================================================
#region "Sprite Setup"

Sprite_Setup
        lda #0
        sta VIC_SPRITE_ENABLE           ; Turn all sprites off
        sta VIC_SPRITE_X_EXTEND         ; clear all extended X bits
        sta SPRITE_POS_X_EXTEND         ; in registers and data

        jsr PlayerInit
        jsr EnemySetup

        lda #%11111111                  ; Turn on sprites 0 1 and 7
        sta VIC_SPRITE_ENABLE 

#endregion 

;===============================================================================
;  MAIN LOOP
;===============================================================================
MainLoop
        jsr WaitFrame                   ; wait for the vertical blank period
        jsr UpdateTimers
        jsr UpdatePlayer                 ; Player animation, etc.
        jsr UpdateScroll
        jsr UpdateEnemy                  ; Enemy animation, etc.
        jsr DisplaySpriteInfoNow              ; Display simple debug info
        ;jsr AnimateStreetLight
        jmp MainLoop

;===============================================================================
; FILES IN GAME PROJECT
;===============================================================================
        incAsm "Collision_Routines.asm"
        incAsm "Game_Interrupts.asm"
        incAsm "Game_Routines.asm"                  ; core framework routines
        incAsm "Player_Routines.asm"
        incAsm "Attacker.asm"
        incAsm "Screen_Memory.asm"                ; screen drawing and handling
        incAsm "Start_Level.asm
        incAsm "Scrolling.asm"
        incAsm "Sprite_Routines.asm"
        incAsm "Attacker_Collision.asm"

;===============================================================================
;  JOYSTICK
;===============================================================================
DisplaySpriteInfo

        jsr admintest
        rts

;******************************************************************************
; Turn off regular display for NOW
; (It is commented out)
;******************************************************************************


;        lda JOY_X
;        bne DisplaySpriteInfoNow
;        lda JOY_Y
;        bne DisplaySpriteInfoNow
;        rts


; 4/5/25
; Implemented this routine to test write out hex values
; For example: First testing consisted of checking if
; ATTRIBUTE2_MEM is reading data into its memory since
; the sprite kept falling through the map (BackAlleyway)

; We will set up a loop here that displays the values
; on the screen in a row as they are seen in memory.
; Note: C64 Debugger may be helpful here later.


DisplaySpriteInfoNow
admintest

;MAP_CHAR_MEM

        lda maptwoloaded
        ldx #19
        ldy #3
        jsr DisplayByte

        lda $4800 + 16
        ldx #19
        ldy #6
        jsr DisplayByte

        lda $4800 + 17
        ldx #19
        ldy #9
        jsr DisplayByte

        lda $4800 + 18
        ldx #19
        ldy #12
        jsr DisplayByte

        lda $4800 + 19
        ldx #19
        ldy #15
        jsr DisplayByte
 
        lda $4800 + 20
        ldx #19
        ldy #18
        jsr DisplayByte

        lda $4800 + 21
        ldx #19
        ldy #21
        jsr DisplayByte

        lda hotelchars
        ldx #20
        ldy #3
        jsr DisplayByte

;        lda filehi
;        ldx #20
;        ldy #21
;        jsr DisplayByte

;        lda MAP_CHAR_MEM + 17
;        ldx #20
;        ldy #9
;        jsr DisplayByte


        loadPointer WPARAM1,SCORE_SCREEN
        rts

        lda ATTRIBUTE2_MEM
        ldx #19
        ldy #3
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 1                          
        ldx #19 
        ldy #6
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 2                          
        ldx #19 
        ldy #9
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 3                          
        ldx #19 
        ldy #12
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 4                          
        ldx #19 
        ldy #15
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 5                          
        ldx #19 
        ldy #18
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 6                          
        ldx #19 
        ldy #21
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 7                          
        ldx #19 
        ldy #24
        jsr DisplayByte
       
        lda ATTRIBUTE2_MEM + 8                          
        ldx #19 
        ldy #27
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 9                          
        ldx #19 
        ldy #30
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 10                          
        ldx #19 
        ldy #33
        jsr DisplayByte

        lda ATTRIBUTE2_MEM + 11                          
        ldx #19 
        ldy #36
        jsr DisplayByte


        loadPointer WPARAM1,SCORE_SCREEN

        rts

; Line 1        
        lda COLLIDER_ATTR
        ldx #19
        ldy #7
        jsr DisplayByte

        lda ActiveTimer
        ldx #2
        ldx #19
        ldy #16
        jsr DisplayByte

        ldx #1
        lda SPRITE_POS_X,x
        ldx #19
        ldy #25
        jsr DisplayByte



; Line 2
        lda ENEMY_COLLIDER_ATTR                           
        ldx #20 
        ldy #7
        jsr DisplayByte

        lda PLAYER_JUMP_POS
        ldx #20
        ldy #16
        jsr DisplayByte

        lda MAP_X_POS
        ldx #20
        ldy #25
        jsr DisplayByte

        ldx #1
        lda MAP_Y_POS
        ldx #20
        ldy #33
        jsr DisplayByte

; Line 3
        ldx #3
        lda SPRITE_POS_X,x
        ldx #22
        ldy #7
        jsr DisplayByte

        lda WaitToFireCD
        ldx #22
        ldy #15
        jsr DisplayByte

        lda FiringHoldCD
        ldx #22
        ldy #23
        jsr DisplayByte

        ldx #2
        lda SPRITE_ANIM_TIMER,x
        ldx #22
        ldy #32
        jsr DisplayByte

; Line 4
        lda gamescore
        ldx #23 
        ldy #6  
        jsr ScoreBoard

        lda ENEMY_BULLETS
        ldx #23
        ldy #23
        jsr DisplayByte

        lda $d010
        ldx #23
        ldy #32
        jsr DisplayByte
        rts

SPRITE_CONSOLE_TEXT
        byte ' coll:$   timr:$   spcx:$   spcy:$      /'
        byte ' enco:$   jump:$   mapx:$   mapy:$      /'
        byte '                                        /'
        byte ' enyx:$   wfir:$: frhd:$   actv:$       /'
        byte ' score:$      bullets:$   msbx:$            ',0

GAME_PANEL_DISPLAY
        byte '            c64brain.com                /'
        byte '                                        /'
        byte '             GAME DEMO                  /'
        byte '                                        /'
        byte '      With multiple scrolling maps       ',0


ADMIN_TEST_DISPLAY
        byte '                                        /'
        byte '                                        /'
        byte '                                        /'
        byte '                                        /'
        byte '                                            ',0

JOY_X                           ; current positon of Joystick(2)
        byte $00                ; -1 0 or +1
JOY_Y
        byte $00                ; -1 0 or +1
NE_DIR
        byte $00
JOY_NW
        byte $00

BUTTON_PRESSED                  ; holds 1 when the button is held down
        byte $00
                                ; holds 1 when a single press is made (button released)
BUTTON_ACTION                   
        byte $00

;-------------------------------------------------------------------------------
; Bit Table
; Take a value from 0 to 7 and return it's bit value
BIT_TABLE
        byte 1,2,4,8,16,32,64,128


;*=$4000
;===============================================================================
; VIC MEMORY BLOCK
; CHARSET AND SPRITE DATA
;===============================================================================
; Charset and Sprite data directly loaded here.

VIC_DATA_INCLUDES

; VIC VIDEO MEMORY LAYOUT - BANK 1 ($4000 - $7FFF)
; SCREEN_1         = $4000 - $43FF         (Screen 0)      ; Double buffered
; SCREEN_2         = $4400 - $47FF         (Screen 1)      ; game screen
; MAP_CHARSET1     = $4800 - $5FFF         (Charset 1)     ; game chars (tiles)
; MAP_CHARSET2     = $5000 - $5FFF         (Charset 1)     ; game chars (tiles)
; SCORE_CHARS      = $5800 - $57FF         (Charset 2)     ; Scoreboard chars
; SCORE_SCREEN     = $5800 - $5BFF         (Screen 6)      ; Scoreboard Screen
; SPRITES          = $5COO - $7FFF         (144 Sprite Images)

;*******************************************************************************
;                             HOTEL MAP
;*******************************************************************************

;*=$4800
MAP_CHAR_MEM                            ; Character set for map screen
;incbin"Maps2025/GameProjectHomeDay3fbk - Chars.bin"
;incbin"Maps2025/GameProjectHomeDay3fTextMulticolor - Chars.bin"

*=$5000

*=$5800
SCORE_CHAR_MEM
incbin "ScoreChars.cst",0,255           ; Character set for scoreboard

*=$5C00

incbin "yoursprite.spt",1,4,true        ; idle (28,33)
incbin "yoursprite.spt",5,6,true
incbin "yoursprite.spt",7,12,true        ; rope climb (36-39)
incbin "yoursprite.spt",13,18,true       ; Walking left (14-27)
incbin "yoursprite.spt",19,24,true       ; Walking right (0 - 13)
incbin "yoursprite.spt",25,28,true       ; Punching to the right
incbin "yoursprite.spt",29,32,true       ; Punching to the left
incbin "yoursprite.spt",33,34,true       ; Fighting
incbin "yoursprite.spt",35,38,true       ; Kicking to the right
incbin "yoursprite.spt",39,42,true       ; Kicking to the left
incbin "yoursprite.spt",43,43,true       ; Unused
incbin "yoursprite.spt",44,46,true       ; Swimming to the right
incbin "yoursprite.spt",47,50,true       ; Swimming to the left
incbin "yoursprite.spt",51,52,true       ; Player (gun to the right)
incbin "yoursprite.spt",53,54,true       ; Player (gun to the left)
incbin "yoursprite.spt",55,55,true       ; Enemy bullet
incbin "yoursprite.spt",56,66,true       ; Unused
incbin "yoursprite.spt",67,68,true       ; Enemy shooting left
incbin "yoursprite.spt",69,74,true       ; Enemy running left
incbin "yoursprite.spt",75,80,true       ; Enemy running right
incbin "yoursprite.spt",81,82,true       ; Enemy shooting right
incbin "yoursprite.spt",83,84,true       ; Dead sprite to left
incbin "yoursprite.spt",85,88,true       ; Enemy attack to right
incbin "yoursprite.spt",89,92,true       ; Enemy attack to left
incbin "yoursprite.spt",93,94,true       ; Unused
incbin "yoursprite.spt",95,96,true       ; Dead sprite to left
incbin "yoursprite.spt",97,98,true       ; Dead sprite to left
incbin "yoursprite.spt",99,104,true      ; Enemy front walking
incbin "yoursprite.spt",105,110,true     ; Enemy back walking

;===============================================================================
;  LEVEL DATA
;===============================================================================

$8000
MAP_MEM
;incbin"Maps2025/GameProjectHomeDay3fbk - Map (22x21).bin"
;incbin"Maps2025/GameProjectHomeDay3fTextMulticolor - Map (22x21).bin"

;incbin"Maps2025/Hotel3l - Map (16x10).bin"
        byte $00,$2E,$15,$29,$00,$2E,$29,$00,$2E,$00,$29,$00,$2E,$29,$15,$29
        byte $26,$25,$19,$29,$25,$26,$25,$25,$26,$25,$29,$25,$26,$29,$15,$29
        byte $28,$2B,$18,$29,$00,$00,$00,$28,$2B,$27,$29,$28,$2B,$29,$15,$29
        byte $08,$08,$1A,$08,$08,$2F,$04,$08,$08,$08,$08,$08,$08,$08,$1A,$08
        byte $2D,$2D,$17,$2D,$0B,$01,$2D,$1E,$1D,$1F,$2D,$2D,$2D,$2D,$17,$2D
        byte $24,$24,$16,$0C,$03,$06,$24,$1C,$1B,$20,$23,$12,$13,$23,$16,$23
        byte $22,$22,$0D,$02,$07,$0E,$22,$22,$22,$22,$22,$22,$22,$22,$22,$22
        byte $14,$14,$05,$14,$14,$0F,$0F,$2A,$10,$11,$2A,$0F,$14,$14,$14,$14
        byte $21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21
        byte $21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21,$21


; Hotel Map Colors
ATTRIBUTE_MEM
;incbin"Maps2025/GameProjectHomeDay3fbk - CharAttribs_L1.bin"
;incbin"Maps2025/GameProjectHomeDay3fTextMulticolor - CharAttribs_L1.bin"

;incbin"Maps2025/Hotel3l - CharAttribs_L1.bin"
        byte $03,$0A,$0A,$0A,$0A,$3E,$0A,$0A,$0A,$0A,$0A,$0A,$0E,$0E,$0E,$0E
        byte $0E,$0E,$0E,$0E,$1E,$0A,$0A,$0A,$1E,$0B,$0A,$0A,$0A,$0A,$0A,$0A
        byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0E,$0E,$0E,$0E,$0C
        byte $0C,$0C,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$18,$0C,$0C,$08,$0C,$0C,$0C
        byte $0B,$0E,$0E,$0E,$0C,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E,$0A,$0A
        byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0E,$0E,$0E,$0E,$0E
        byte $0E,$0E,$0E,$0E,$0E,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A
        byte $0A,$0A,$0A,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0A,$0A,$0A,$0D,$0D
        byte $0D,$0D,$0D,$0A,$0A,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$0A,$8C,$8C
        byte $8C,$8C,$8C,$0A,$0E,$09,$0E,$09,$0E,$09,$0E,$0A,$09,$0E,$09,$0E
        byte $0E,$0C,$0C,$0C,$0E,$0E,$0E,$0A,$0E,$0E,$0A,$0A,$0A,$0A,$0A,$0E
        byte $0E,$0A,$0A,$0A,$0A,$0A,$0B,$0A,$0A,$0B,$0A,$0A,$0A,$0A,$0F,$0F
        byte $0E,$0A,$0A,$0A,$0A,$0D,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$1E,$0E,$0E
        byte $0C,$0C,$0C,$0C,$0B,$0B,$0C,$0A,$0D,$0A,$0A,$0E,$0E,$0E,$0A,$0A
        byte $0A,$0E,$0E,$0C,$0C,$0A,$0E,$0E,$0E,$0E,$0C,$0C,$0A,$0A,$0A,$0A
        byte $0A,$0A,$0A,$0A,$0E,$0C,$0B,$0B,$0B
;incbin"Maps2025/GameProjectHomeDay3fbk - Tiles.bin"
;incbin"Maps2025/GameProjectHomeDay3fTextMulticolor - Tiles.bin"

;incbin"Maps2025/Hotel3l - Tiles.bin"

TILE_MEM
        byte $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        byte $02,$03,$04,$05,$06,$06,$05,$07,$06,$05,$08,$09,$05,$0A,$06,$0B
        byte $0C,$0D,$0E,$05,$0C,$0F,$05,$10,$0E,$05,$11,$12,$05,$13,$12,$14
        byte $06,$15,$06,$05,$06,$16,$05,$17,$0E,$05,$11,$12,$05,$13,$12,$18
        byte $19,$19,$19,$05,$01,$01,$05,$1A,$01,$05,$1B,$01,$05,$1C,$1D,$1D
        byte $1E,$1F,$06,$05,$20,$21,$05,$22,$23,$05,$24,$25,$05,$1C,$26,$27
        byte $28,$06,$06,$29,$2A,$06,$06,$29,$18,$18,$18,$2B,$18,$18,$18,$2B
        byte $2C,$2D,$18,$18,$2E,$18,$18,$18,$14,$14,$14,$14,$14,$14,$14,$14
        byte $2F,$30,$30,$31,$32,$33,$34,$35,$36,$37,$38,$37,$39,$3A,$3B,$3C
        byte $3D,$3E,$3E,$3F,$40,$41,$41,$42,$41,$41,$40,$42,$43,$43,$44,$45
        byte $3D,$3E,$3E,$3F,$40,$46,$47,$48,$49,$4A,$40,$4B,$4C,$43,$44,$4D
        byte $4E,$4F,$50,$51,$06,$52,$53,$54,$55,$16,$06,$54,$56,$15,$06,$54
        byte $06,$06,$57,$58,$06,$59,$06,$5A,$5B,$5C,$18,$5D,$5E,$0D,$18,$5D
        byte $18,$18,$18,$5F,$18,$18,$60,$61,$14,$62,$63,$18,$14,$5F,$64,$18
        byte $18,$18,$18,$2B,$18,$18,$18,$2B,$14,$14,$14,$2B,$14,$14,$14,$2B
        byte $65,$65,$65,$66,$67,$67,$67,$68,$69,$6A,$25,$6B,$6C,$6D,$6E,$6F
        byte $65,$70,$71,$71,$72,$73,$74,$75,$76,$77,$78,$78,$79,$7A,$7A,$7A
        byte $7B,$7B,$7C,$7D,$7E,$74,$7F,$72,$78,$78,$80,$81,$7A,$7A,$7A,$82
        byte $06,$83,$84,$84,$06,$85,$86,$87,$88,$89,$8A,$8A,$8B,$8C,$8C,$8C
        byte $84,$84,$8D,$06,$8E,$86,$8F,$06,$8A,$8A,$90,$91,$8C,$8C,$8C,$92
        byte $93,$65,$65,$65,$67,$67,$67,$67,$6A,$25,$6A,$25,$6D,$6E,$6D,$6E
        byte $94,$95,$96,$97,$98,$99,$9A,$97,$94,$95,$96,$40,$98,$99,$9A,$40
        byte $94,$95,$96,$9B,$98,$99,$9A,$9B,$94,$95,$9C,$40,$9D,$9E,$9F,$A0
        byte $94,$95,$96,$9B,$98,$99,$9A,$9B,$94,$95,$96,$9B,$98,$99,$9A,$9B
        byte $94,$95,$96,$A1,$98,$99,$9A,$A1,$94,$95,$96,$A1,$98,$99,$9A,$40
        byte $94,$95,$96,$A2,$98,$99,$9A,$A3,$94,$95,$96,$A2,$98,$99,$9A,$A3
        byte $94,$95,$96,$A4,$98,$99,$9A,$A5,$94,$95,$96,$A6,$98,$99,$9A,$40
        byte $A7,$A8,$A9,$A7,$AA,$AA,$AA,$AA,$AB,$AB,$AC,$AC,$AD,$AD,$AD,$AD
        byte $06,$AE,$AF,$B0,$06,$AA,$AA,$AA,$0C,$B1,$AC,$AC,$0C,$B2,$AD,$AD
        byte $B3,$B3,$B3,$B4,$06,$06,$06,$06,$B5,$B6,$B6,$B7,$B8,$B9,$B9,$BA
        byte $BB,$BC,$BC,$BC,$BD,$BD,$BD,$BD,$BD,$BD,$BE,$BF,$BD,$BD,$AF,$C0
        byte $B3,$B3,$B3,$B4,$06,$06,$06,$06,$C1,$C2,$C3,$06,$C4,$C5,$C6,$06
        byte $AF,$B0,$C7,$06,$AA,$AA,$AA,$C8,$AC,$AC,$C9,$CA,$AD,$AD,$CB,$CC
        byte $18,$18,$18,$18,$14,$14,$14,$14,$14,$14,$14,$14,$CD,$CD,$CD,$CD
        byte $18,$18,$18,$18,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14,$14
        byte $06,$06,$06,$06,$06,$06,$06,$06,$40,$CE,$CF,$18,$CF,$18,$18,$18
        byte $06,$06,$06,$06,$06,$06,$06,$06,$18,$18,$18,$18,$18,$18,$18,$18
        byte $D0,$D1,$D0,$D1,$D2,$D3,$D2,$D3,$D3,$D2,$D3,$D2,$D2,$D3,$D2,$D3
        byte $D0,$D1,$D0,$D1,$D2,$D4,$D5,$D6,$D6,$D2,$D6,$D2,$D6,$D2,$D6,$D2
        byte $A1,$A1,$A1,$A1,$40,$D7,$40,$D8,$D9,$DA,$97,$97,$DB,$DC,$DD,$DD
        byte $A1,$A1,$A1,$A1,$D8,$40,$DE,$40,$97,$97,$DF,$E0,$DD,$DD,$E1,$E2
        byte $A1,$E3,$E4,$A1,$D8,$E3,$E4,$40,$97,$E3,$E4,$97,$D8,$E3,$E4,$A1
        byte $65,$65,$65,$65,$72,$E5,$72,$72,$40,$E6,$E7,$40,$44,$E8,$E9,$44
        byte $A1,$A1,$A1,$A1,$01,$EA,$EB,$40,$EC,$ED,$EE,$EF,$44,$F0,$F1,$44
        byte $06,$06,$06,$F2,$06,$06,$06,$F2,$06,$06,$06,$F2,$06,$06,$06,$F2
        byte $F3,$B4,$4F,$4F,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
        byte $00,$F4,$F5,$00,$40,$F6,$F7,$40,$00,$F8,$F8,$40,$00,$00,$00,$00
        byte $19,$19,$19,$19,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;*******************************************************************************
;                          BACK ALLEY MAP
;*******************************************************************************

;charset_data
;* = $4800
MAP2_CHAR_MEM 

*=$8000
MAP2_MEM
        byte $25,$00,$02,$02,$02,$02,$02,$20,$00,$25,$00,$00,$24,$00,$00,$25
        byte $00,$00,$00,$01,$1F,$03,$1F,$03,$21,$00,$00,$00,$23,$23,$00,$00
        byte $00,$00,$22,$00,$02,$02,$02,$02,$02,$20,$22,$00,$00,$23,$23,$00
        byte $0A,$25,$08,$0D,$00,$01,$1F,$04,$1F,$03,$21,$0D,$00,$00,$23,$23
        byte $00,$0A,$00,$07,$0E,$06,$06,$06,$05,$06,$06,$06,$0E,$19,$1A,$10
        byte $0F,$0F,$26,$27,$0F,$12,$11,$13,$11,$12,$16,$17,$18,$11,$11,$11
        byte $11,$12,$13,$11,$11,$12,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15
        byte $15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$15
        byte $15,$15,$15,$15,$15,$15,$15,$15,$1E,$1B,$1E,$1E,$1E,$1B,$1E,$1E
        byte $1E,$1B,$1E,$1E,$15,$15,$15,$15,$1E,$1D,$1C,$1D,$1C,$1D,$1C,$1D
        byte $1C,$1D,$1C,$1D,$1C,$1D,$1C,$1D,$1C,$1D

;color_data
ATTRIBUTE2_MEM
        byte $03,$0C,$0A,$0A,$0A,$0A,$0A,$0A,$0E,$0E,$0E,$0E,$0A,$0A,$0A,$0A
        byte $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$09,$0B
        byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0E,$0E
        byte $0E,$0A,$0A,$0A,$0A,$0A,$0A,$0F,$0A,$0A,$0A,$0F,$0A,$0F,$0F,$0A
        byte $0A,$0A,$0A,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0E,$0E,$0E,$0B
        byte $0E,$0B,$0B,$0B,$0E,$0B,$0B,$0B,$0E,$0B,$0B,$0B,$0E,$0B,$0B,$0B
        byte $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B
        byte $0B,$0B,$0B,$1A,$1A,$1A,$1A,$1A,$1A,$1F,$1A,$1F,$1A,$1A,$1A,$1E
        byte $1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1A,$1A,$1A,$1A,$1E,$1E,$1E,$1E
        byte $1E,$1A,$1A,$1A,$1E,$1A,$1E,$1E,$1E,$1A,$0E,$0E,$5E,$0E,$0D,$0D
        byte $5E,$5E,$5E,$5E,$5E,$5E,$5E,$5E,$0E,$0C,$0D,$0E,$0E,$0E,$0E,$0E
        byte $0E,$0E,$0E,$0B,$0B,$0A,$0A,$09,$09,$09,$09,$09,$0E,$0E,$0F,$0E
        byte $0E,$0E,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0E,$0A,$0E,$0A
        byte $0A,$0E,$0A,$0A,$0E,$0A,$0E,$0E,$0E,$09,$0E,$0E,$0E,$0E,$0E,$0E
        byte $0E,$0E,$0E,$0E,$0F,$0B,$0B,$09,$0E,$0E,$0E,$0C,$0C,$0A,$09,$09
        byte $0D,$0E,$0E,$0E,$0C,$0C,$0A


;tiles_data
TILE2_MEM
        byte $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        byte $02,$03,$04,$05,$02,$02,$06,$07,$02,$03,$04,$05,$02,$02,$06,$07
        byte $08,$09,$0A,$0B,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F,$02,$03,$10,$05
        byte $02,$03,$10,$05,$02,$02,$10,$05,$02,$03,$10,$05,$02,$03,$10,$05
        byte $11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$18,$15,$1C,$1D,$18
        byte $1E,$1F,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D
        byte $2E,$2F,$2F,$30,$31,$32,$32,$33,$31,$32,$32,$33,$34,$35,$35,$36
        byte $37,$38,$38,$39,$3A,$3B,$3B,$3C,$3A,$3B,$3D,$3C,$3A,$3E,$3F,$3C
        byte $37,$40,$41,$39,$3A,$42,$43,$3C,$3A,$3B,$3D,$3C,$3A,$3E,$3F,$3C
        byte $44,$45,$46,$47,$44,$45,$46,$47,$44,$45,$46,$47,$44,$45,$46,$47
        byte $48,$49,$49,$4A,$49,$4B,$49,$4A,$49,$49,$48,$4A,$49,$49,$49,$4A
        byte $44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44,$44
        byte $44,$4C,$44,$44,$44,$4C,$44,$44,$44,$4C,$44,$44,$4D,$4E,$44,$44
        byte $44,$4C,$44,$44,$44,$4C,$44,$44,$44,$4C,$44,$44,$44,$4C,$44,$44
        byte $4F,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E
        byte $5F,$60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$5D,$6D
        byte $4F,$6E,$51,$6F,$53,$70,$55,$71,$57,$72,$59,$5A,$5B,$6C,$5D,$5E
        byte $73,$73,$73,$73,$74,$74,$74,$74,$74,$74,$74,$74,$75,$76,$76,$77
        byte $73,$73,$73,$73,$74,$74,$74,$74,$74,$74,$74,$74,$78,$78,$78,$78
        byte $73,$73,$73,$73,$74,$74,$74,$74,$74,$79,$74,$74,$7A,$7B,$7A,$7A
        byte $73,$73,$73,$73,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74,$74
        byte $7C,$04,$7C,$04,$7D,$7C,$04,$7C,$7C,$04,$7C,$04,$04,$7C,$04,$7C
        byte $73,$73,$7E,$7F,$74,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A
        byte $7F,$7F,$7F,$7F,$8B,$8C,$8B,$8C,$86,$86,$8D,$8E,$8F,$90,$91,$92
        byte $7F,$93,$94,$94,$82,$95,$74,$74,$96,$97,$74,$74,$8F,$90,$39,$98
        byte $99,$9A,$9A,$9A,$9B,$9C,$9D,$9E,$9F,$A0,$A1,$A2,$A3,$A4,$A5,$A6
        byte $9A,$9A,$9A,$A7,$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AB,$AF,$AF,$B0,$B1
        byte $B2,$B2,$B2,$B2,$B3,$B4,$B5,$B3,$B3,$B3,$B3,$B3,$B3,$B3,$B3,$B3
        byte $B6,$B6,$B6,$B6,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7
        byte $B6,$B6,$B6,$B8,$B7,$B7,$B9,$BA,$B7,$B9,$BA,$B7,$B9,$BA,$B7,$B7
        byte $B2,$B2,$B2,$B2,$B3,$B3,$B3,$B3,$B3,$B3,$B3,$B3,$B3,$B3,$B3,$B3
        byte $02,$03,$04,$05,$02,$BB,$BC,$07,$02,$BD,$BE,$05,$02,$BF,$C0,$07
        byte $02,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$02,$CC,$C2,$C3
        byte $02,$CD,$CE,$CF,$02,$D0,$D1,$D2,$02,$D0,$CE,$C3,$02,$D3,$D1,$D4
        byte $00,$D5,$D6,$D7,$44,$4C,$00,$D8,$44,$4C,$00,$00,$00,$4C,$00,$00
        byte $D9,$DA,$D9,$DB,$DC,$DD,$DC,$DE,$D9,$DA,$D9,$DF,$DC,$DD,$DC,$E0
        byte $00,$00,$00,$00,$00,$44,$44,$00,$00,$44,$44,$00,$00,$E1,$E2,$44
        byte $00,$00,$00,$00,$00,$E3,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        byte $E4,$60,$61,$62,$E5,$64,$55,$E6,$57,$E7,$E8,$E9,$5B,$88,$EA,$EB
        byte $EC,$60,$61,$62,$ED,$EE,$EF,$71,$F0,$E9,$F1,$F2,$F3,$F4,$91,$F5
        byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

debugger byte 1


