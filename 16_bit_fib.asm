;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KATAR YAZ ;;;;;;;;;;;;;;;;;;;;;;;;
KATAR_YAZ macro katar_offset     ; katar_offset katarin efektif adresi
    push dx
    push ax
    
    mov dx, offset katar_offset             
    mov ah, 09h
    int 21h
    
    pop ax
    pop dx
endm                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KATAR YAZ ;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; BASAMAK_HESAPLA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BASAMAK_HESAPLA macro liste_offset sayi_offset
    push ax             ;
    push bx             ;
    push cx             ;  Registerlerin onceki degerlerini koru
    push dx             ;
    
    
    xor cx, cx ; cx'i temizle
    xor dx, dx ; dx'i temizle
    xor ax, ax ; ax'i temizle
    xor bx, bx ; bx'i temizle
    
    mov si, offset liste_offset             
    mov di, offset sayi_offset
    
    mov cl, [si+1]
   
    hesapla:
        mov al, [si+2]
        
        cmp al, 48                          ; 0'a esit ya da buyuk mu ?            ;
        jl hatali_girdi                                                            ;
                                                                                   ;  amac karakterin rakam oldugunu kontrol etmek
        cmp al, 57                          ; 9'a esit ya da kucuk mu ?            ;
        jg hatali_girdi                                                            ;
        
        sub al, 48                          ; girilen karakteri rakama donustur ?  ;
                                                                                   ;
                                                                                   ;
        cmp cl, 0                           ; rakam sayisi 0 ise                   ;
        je sifira                                                                  ;
                                                                                   ; 
        cmp cl, 1                           ; rakam sayisi 1 ise                   ;
        je bire                                                                    ;
                                                                                   ;
        cmp cl, 2                           ; rakam sayisi 2 ise                   ;   Amac ilgili rakamin hanesini bulma
        je ikiye                                                                   ;
                                                                                   ;
        cmp cl, 3                           ; rakam sayisi 3 ise                   ;
        je uce                                                                     ;
                                                                                   ;
        cmp cl, 4                           ; rakam sayisi 4 ise                   ;
        je dorde                                                                   ;
                                                                                   ;
        cmp cl, 5                           ; rakam sayisi 5 ise                   ;
        je bese
        
        bese:                                                                      ;
           xor bx, bx                                                              ;
           xor dx, dx                                                              ;
           mov bx, offset ONBIN                                                    ;
           TOPLA                                                                   ;
        dorde:                                                                     ;
           xor bx, bx                                                              ;
           xor dx, dx                                                              ;
           mov bx, offset BIN                                                      ;
           TOPLA                                                                   ;
        uce:                                                                       ;
           xor bx, bx                                                              ;
           xor dx, dx                                                              ;
           mov bx, offset YUZ                                                      ;       Amac ilgili rakamin hanesiyle carpip girilen sayiyi bulmak
           TOPLA                                                                   ;
        ikiye:                                                                     ;
           xor bx, bx                                                              ;
           xor dx, dx                                                              ;
           mov bx, offset ON                                                       ;
           TOPLA                                                                   ;
                                                                                   ;
        bire:                                                                      ;
           add byte ptr[di], al                                                    ;
           jmp sifira                                                              ;
                                                                                   ;
        sifira:                                                                    ;
        xor ax, ax                                                                 ;
        xor dx, dx                                                                 ;
                                                                                   ;
        inc si                                                                     ;
    loop hesapla                                                                   ;
    
    pop dx        ;
    pop cx        ; 
    pop bx        ;  korunmus degerleri yigindan geri al
    pop ax        ;
                  
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TOPLA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Bu macro byte'a gore carpma ve toplama islemini gerceklestirmektedir;;;;;;;;;;;
TOPLA macro
    
   push ax                          ; ax'in degerini koru
   mul byte ptr [bx]                ; byte kadar carpma islemi yap
   add byte ptr[di], al             ; al yi sayinin ilk bayte'ina ekle
   adc byte ptr[di+1], ah           ; ah'daki degeri(byte * 4 bit = en fazla 2 byte(16 bit)) sayini ikinci bayte'ina carry ile beraber ekle.
   pop ax                           ; ax'in onceki degerini yigindan al.

   push ax
   mul byte ptr [bx+1]
   add byte ptr[di+1], al
   adc byte ptr[di+2], ah
   pop ax
   jmp sifira                       

endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TOPLA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BASAMAK_HESAPLA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
org 100h
     
    .data                                   ; Degiskenlerin tanimlandigi yer
        ON              dw 10               ;
        YUZ             dw 100              ;
        BIN             dw 1000             ;
        ONBIN           dw 10000            ;
        
        KULLANICI_TAM_SAYI_GIR_SORGUSU  db  'Lutfen bir sayi giriniz(Max: 4.100.200.300):','$'
        HATA     db 'Hatali girdi, !!!!!','$'; HATALI input ciktisi
        SAYI     dw ?                       ; kullanicinin girdigi sayi
        BUFF     db  6                      ; olabilecek max rakam (11).
                 db  ?                      ; kullanicinin girdigi rakamlarin sayini tutacak.
                 db  6 dup(0)               ; girilen karakterler. 
        FIB1 dw 1                           ; Fibonacci n-1 icin
        FIB0 dw 0                           ; Fibonacci n-2 icin
        SAYAC   dw 0                        ; Kac tane fibonacci sayisi var ?
        LPARA db 'F(',0                     ; 
	    RPARA db '): ',0                    ;
	    ASALLIK db ' (Asal)',0              ;                                  
	    RAKAMLAR DB 10 DUP(0)               ;sayinin rakamlarini tutacak dizi
        N DW 0                              ; BASAMAK SAYISI        
        YEDI DB 1,3,2,-1,-3,-2,1,3,2,-1     ; yedi'nin bolunebilme kurali icin 
        INDIS DB 0                          ; 
          
        SATIRBASI DB CR,LF, '',0 
        CR EQU 13  ; SATIRBASI ICIN
	    LF EQU 10  ;    GEREKLI
    .code
        
        mov ax, @data
        mov ds, ax
        mov es, ax
        
        sayi_al:
            KATAR_YAZ KULLANICI_TAM_SAYI_GIR_SORGUSU            ; Kullanicidan girdi isteme
            call SATIR_ATLA                                     ; yeni satira gec
            mov ah, 0Ah                                         ; Kullanicidan string oku
            mov dx, offset BUFF                                 ; dx'e BUFF in efektif adresini ata
            int 21h;                                            ; Girdi icin interrupt olustur
            
            BASAMAK_HESAPLA BUFF SAYI                           ; Kullanicidan girdilerin integer karsiligini bul
            call SATIR_ATLA                                     ; yeni satira gec
            jmp fib                                             ; fibonacci sayilarini bulan fib etiketine dallan

        hatali_girdi:
            call SATIR_ATLA                                     ; yeni satira gec
            KATAR_YAZ HATA                                      ; hatayi yaz
            call SATIR_ATLA                                     ; yeni satira gec
            jmp sayi_al                                         ; tekrar kullanicidan sayi almak uzere sayi al etiketine dallan.
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    FIB:  
    mov ax, FIB1      ;
    cmp SAYI, ax      ; fibonacci < girilen sayidan daha kucuk olup olmadigini kontrol et
    jb FIB_SON        ;

    
    mov bx, FIB1      ;   TEMP = FIB1  BX = temp ; 
    mov ax, FIB0      ;                          ;    n - 1 fibonacciyi korumak icin bx'i gecici degisken olarak kullan
    add FIB1, ax      ;   FIB1 = FIB0            ;
    mov FIB0, bx      ;   FIB0 = TEMP            ;
    
    mov ax, OFFSET LPARA                         ;
    call YAZDIR                                  ;
    mov AX, SAYAC        ; SAYAC'i ax'e ata      ;
    CALL PUTN            ; SAYAC'i yazdir                      ;  amac  ' F(SAYAB) :  Fibonacci ' yi yazdirmak
    MOV AX, OFFSET RPARA                         ;
    CALL YAZDIR                                  ;
    MOV AX, FIB0         ; FIB0'i ax'e ata       ;
    CALL PUTN            ; n'inci fibonacci sayisni  yazdir
    
    CMP FIB0,1           ;1 ise asal
    JE ASAL              ; asal yazmak uzere asal etketine dallan
 
    CMP FIB0,2           ; 2 ISE ASAL
    JE ASAL              ; asal yazmak uzere asal etketine dallan
    
    XOR DX,DX     ; dx'i temizle
    MOV AX, FIB0  ; fib0'i ax'e ata
    MOV CX, 2     ;
    DIV CX        ;    2'YE BOLME
    CMP DX,0      ;    kalan 0 ise  
    JE NOT_ASAL   ;    asal degil ve asal_degil etiketine dallan
      
      
       
    MOV N,0                    ;
    MOV AX, FIB0               ;
    XOR SI,SI                  ;
    basamak:                   ;
        XOR DX,DX              ;   amac n'inci fibonacci sayisinin rakamlarini bulmak 
        MOV CX, 10             ;
        DIV CX                 ;       
        MOV RAKAMLAR[SI], DL   ;       
        INC N                  ; basamak sayisini tutan degiskeni bir arttir
        INC SI                 ; adresi ilerlet
        CMP AX,0               ; son basamak mi sayi sifirdan buyuk ise
        JA basamak             ;
           
           
    
    CMP FIB0,3            ; fibonacci 3 ise esit ise
    JE ASAL               ; asal etiketine dallan
                          ;
    xor SI,SI             ; si'yi temizle
    xor AX,AX             ;    
    MOV CX, N             ; basamak sayisini tutan degiskeni cx'e ata ; amac basamaklarda gezinlelim ve toplamlarini bolunebilme kurallari icin kullanalim
basamak_topla:                     ;
    ADD AL, RAKAMLAR[SI]  ;
    INC SI                ; 
    LOOP basamak_topla    ;         
                          ;
    MOV CH, 3             ;  3 icin  bolunebilme kurali
    DIV CH                ;
    CMP AH, 0             ;
    JE NOT_ASAL           ;
    
    
    
    CMP FIB0,5            ; fibonacci 5'e esit ise
    JE ASAL               ; asal etiketine dallan
                          ;
    CMP RAKAMLAR[0], 0    ; RAKAMLAR dizisi fibonacci sayinin tum basamak rakamlarini tuttuguna gore
    JE NOT_ASAL           ;
    CMP RAKAMLAR[0], 5    ; 5 icin  bolunebilme kurali     
    JE NOT_ASAL           ;
                                       
     
     
    CMP FIB0,7            ; fibonacci 7'ye esit ise
    JE ASAL               ; asal etiketine dallan
                          ;
    XOR AX,AX             ;
    XOR BX,BX             ;
    XOR SI,SI             ;
    XOR DI,DI             ;     
	MOV DI, OFFSET YEDI   ; DIZI 
                          ;
    MOV CX, N             ; N basamak kadar gezin
gezin:                    ;                       
    PUSH CX               ; cx'i koru
    MOV AL,RAKAMLAR[SI]   ; basamaktaki sayiyi al'e kopyala                           [SI]
    MOV CL,YEDI[SI]       ; yedi dizisinden ilgili basamaga karsilik gelen carpani al [SI]
    IMUL CL               ; 
    ADD BX,AX             ;    7 icin  bolunebilme kurali
    INC SI                ;
    INC DI                ;  YEDI dizisindeki sonraki sayiyi elde etmek icin di'yi bir arttir
    POP CX                ;  cx'i geri al - dongunun N kadar calismasi icin push pop edildi.
    LOOP gezin            ;  
                          ;
    MOV AX,BX             ;
    MOV CH,7              ;   7 icin  bolunebilme kurali
    IDIV CH               ;
                          ;
    CMP AH, 0             ;
    JE NOT_ASAL           ;
                            
                            
                            
    CMP FIB0,11           ; fibonacci 11'e esit ise
    JE ASAL               ; asal etiketine dallan
     
    XOR SI,SI             ;      
	MOV CX, N             ;
for:                      ;
    PUSH CX               ;
    XOR AH,AH             ;         
    MOV AL,INDIS          ;
    MOV CH,2              ; INDIS -> amac -> n'inci basamagin cift mi tek mi diye anlamak +-+-...
    DIV CH                ;
    CMP AH,0              ; INDIS sifir ya da cift ise + pozitif olarak degerlendir
    JE pozitif            ;
    SBB BL,RAKAMLAR[SI]   ; yoksa BL(pozitiflari tutan register)'den cikar 
    JMP sonraki           ;  dallan
                          ;
pozitif:                  ; 11'in  bolunme kurali
    ADD BL,RAKAMLAR[SI]   ;
                          ;
sonraki:                  ;
    INC SI                ;
    INC INDIS             ;
    POP CX                ;        
    LOOP for              ;
                          ;
    cmp bl, 0             ;
    jge devam             ;
    add bl, 11            ;
                          ;
    devam:                ;      
        MOV AX,BX         ;             
        mov ch, 11        ;
        div ch            ;
        CMP ah, 0         ;  11'in  bolunme kurali
        JE NOT_ASAL       ;
                                             
    
    
    CMP FIB0,13           ;fibonacci 13'e esit ise
    JE ASAL               ;
    
                            ;
                            ;
                            ;
                            ;
                            ; 13'E BOLUM
                            ;
                            ;
                            ; 
                            ;
                         
    
    
    CMP FIB0,17           ; fibonacci 17'ye esit ise
    JE ASAL               ; asal etiketine dallan
    
    XOR DX,DX             ;
    MOV AX, FIB0          ;   ax -> X
    MOV CX, 10            ;
    DIV CX                ;   X = 10a+b
    MOV BX,AX             ;   bx -> a
                          ;   17'nin  bolunme kurali
    MOV AX,DX             ;   ax -> b 
    MOV CX,5              ;
                          ;
    MUL CX                ;   5*b
    SUB BX,AX             ;   a - 5*b
                          ;
    XOR DX,DX             ;
    MOV AX,BX             ;
    MOV CX,17             ;
    DIV CX                ;
    CMP DX,0              ;
    JE NOT_ASAL           ; 
    
    
    
    CMP FIB0,19           ; fibonacci 19'a esit ise
    JE ASAL               ; asal etiketine dallan
           
    XOR DX,DX       
    MOV AX, FIB0          ;
    MOV CX, 10            ;  ax -> X
    DIV CX                ;  X = 10a+b
    MOV BX,AX             ;  bx -> a
                          ;  19'un  bolunme kurali
    MOV AX,DX             ;  ax -> b 
    MOV CX,2              ;  cx -> 2
    
    MUL CX                ;  2*b
    ADD BX,AX             ;  2*b + a
                          ;
    XOR DX,DX             ;
    MOV AX,BX             ;
    MOV CX,19             ;  19'un  bolunme kurali
    DIV CX                ;
    CMP DX,0              ;
    JE NOT_ASAL           ; 
    
    
    
    CMP FIB0,23           ;  fibonacci 23'e esit ise
    JE ASAL               ;  asal etiketine dallan
           
    XOR DX,DX             ;  23'un  bolunme kurali
    MOV AX, FIB0          ;  ax -> X 
    MOV CX, 10            ;  cx -> 10
    DIV CX                ;  X  = 10*a+b
    MOV BX,AX             ;  bx -> a
                          ; 
    MOV AX,DX             ;  ax -> b
    MOV CX,7              ;  cx -> 7
    
    MUL CX                ;  7*b
    ADD BX,AX             ;  7*b + a
                          ;
    XOR DX,DX             ;
    MOV AX,BX             ;
    MOV CX,23             ;  23'un  bolunme kurali
    DIV CX                ;
    CMP DX,0              ;
    JE NOT_ASAL           ; 

    
    ASAL:
        MOV AX, OFFSET ASALLIK      ;  asal olmasi durumunda 
        CALL YAZDIR                 ;  
    
    NOT_ASAL:    
        MOV AX, OFFSET SATIRBASI    ; asal degilse sayet
        CALL YAZDIR                 ; satir basina gec
        
    INC SAYAC                       ; SAYAC'i bir attir
    JMP FIB
    

FIB_SON:
        
        
        
        
        
        
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                
              
       
ret

SATIR_ATLA  proc near
    push dx
    push ax
    
    mov dl, 0dh                                             ; \r
    mov ah, 02h
    int 21h
    
    mov dl, 0ah                                             ; \n
    mov ah, 02h
    int 21h
    
    pop ax                                                  ; stack oldugu icin tersten pop ediyoruz            
    pop dx
    ret
SATIR_ATLA endp

;SAYI OUTPUT VERME				
PUTN    proc NEAR
        push cx
        push dx
        xor dx,dx
        push dx
        mov cx,10
        cmp ax,0
        jge CALC_DIGITS
CALC_DIGITS:				
        div cx
        add dx,'0'
        push dx
        xor dx,dx
        cmp ax,0
        jne CALC_DIGITS
DISP_LOOP:
        POP ax
        cmp ax,0
        je END_DISP_LOOP
        call PUTC
        JMP DISP_LOOP
END_DISP_LOOP:  
        POP dx
        POP cx
        RET
PUTN    endp 

;KARAKTER OUTPUT VERME
PUTC    proc NEAR
		push ax
		push dx
		mov DL,AL
		mov AH,2
		int 21h
		pop dx
		pop ax
		ret
PUTC    endp

;STRING OUTPUT VERME
YAZDIR proc NEAR
        push BX              ; bx'i koru, yigina atarak
        mov BX,AX            ; bu fonksiyon cagirlimadan ax'e atanan degikeni bx'e kopyala
        mov AL,BYTE PTR [BX] ; bx'in byte kadarlik kismini al'ye ata
PUT_LOOP:
        CMP AL,0
        JE PUT_FIN
        CALL PUTC            ; 
        INC BX
        MOV AL,BYTE PTR [BX]
        JMP PUT_LOOP
PUT_FIN:
        POP BX
        RET
YAZDIR ENDP  
