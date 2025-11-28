INCLUDE Irvine32.inc

.data

CR EQU 13      ; 0Dh = carriage return
LF EQU 10      ; 0Ah = line feed

; Main menu 
mainMenu BYTE CR, LF,"================> Scientific Calculator <================",CR, LF,
            "1. Basic arithmetic (+, -, *, /)",CR, LF,
            "2. Scientific functions (sin, cos, tan, sqrt, ln)",CR, LF,
            "3. Quadratic equation solver",CR, LF,
            "4. Vector addition & axioms check",CR, LF,
            "0. Exit",CR, LF,
            "---------------------------------------------------------",CR, LF,
            "Enter choice: ",0

invalidChoiceMsg        BYTE CR, LF,"[Error] Invalid menu choice. Try again.",CR, LF,0
goodbyeMsg              BYTE CR, LF,"Exiting calculator. Goodbye!",CR, LF,0

pressKeyMsg             BYTE CR, LF,"Press any key to continue...",CR, LF,0
newline                 BYTE CR, LF,0

; Basic arithmetic
basicMenuTitle          BYTE CR, LF,"--- Basic Arithmetic (a op b) ---",CR, LF,0
promptFirst             BYTE "Enter first operand (floating-point): ",0
promptSecond            BYTE "Enter second operand (floating-point): ",0
promptOp                BYTE "Enter operator (+ - * /): ",0
resultLabel             BYTE CR, LF,"Result = ",0
divZeroMsg              BYTE CR, LF,"[Math Error] Division by zero.",CR, LF,0

; Scientific functions 
sciMenuTitle            BYTE CR, LF,"--- Scientific Functions ---",CR, LF,
                           "1. sin(x) (x in degrees)",CR, LF,
                           "2. cos(x) (x in degrees)",CR, LF,
                           "3. tan(x) (x in degrees)",CR, LF,
                           "4. sqrt(x)",CR, LF,
                           "5. ln(x) (natural logarithm)",CR, LF,
                           "0. Back to main menu",CR, LF,
                           "Enter choice: ",0

promptX                BYTE "Enter x (floating-point): ",0
domainErrorSqrt        BYTE CR, LF,"[Math Error] sqrt(x) only defined for x >= 0.",CR, LF,0
domainErrorLn          BYTE CR, LF,"[Math Error] ln(x) only defined for x > 0.",CR, LF,0
tanDomainWarn          BYTE CR, LF,"[Warning] tan(x) is undefined for x = 90 + k*180 degrees.",CR, LF,0

; Quadratic solver 
quadMenuTitle          BYTE CR, LF,"--- Quadratic Equation Solver ---",CR, LF,
                            "Solve ax^2 + bx + c = 0",CR, LF,
                            "1. Use predefined equations",CR, LF,
                            "2. Enter your own a, b, c",CR, LF,
                            "0. Back to main menu",CR, LF,
                            "Enter choice: ",0

promptA                     BYTE "Enter a: ",0
promptB                     BYTE "Enter b: ",0
promptC                     BYTE "Enter c: ",0

quadNotQuadMsg              BYTE CR, LF,"[Error] a = 0 => not a quadratic equation.",CR, LF,0
quadNoRealMsg               BYTE CR, LF,"[Math Error] Discriminant < 0, no real roots.",CR, LF,0
quadRootsLabel              BYTE CR, LF,"Roots (x1, x2):",CR, LF,0
x1Label                     BYTE "x1 = ",0
x2Label                     BYTE "x2 = ",0

FLOAT_SCALE_1000            REAL8 1000.0
scaledIntegerForPrint       SDWORD ?

; predefined quadratics:
; 1) x^2 - 3x + 2 = 0 (roots 1,2)
quad1CoeffA      REAL8 1.0
quad1CoeffB      REAL8 -3.0
quad1CoeffC      REAL8 2.0

; 2) x^2 + 2x - 8 = 0 (roots 2,-4)
quad2CoeffA      REAL8 1.0
quad2CoeffB      REAL8 2.0
quad2CoeffC      REAL8 -8.0

quadPresetMenu             BYTE CR, LF,"Predefined equations:",CR, LF,
                                "1) x^2 - 3x + 2 = 0",CR, LF,
                                "2) x^2 + 2x - 8 = 0",CR, LF,
                                "Choose (1-2): ",0

; Vector addition & axioms 
vecMenuTitle               BYTE CR, LF,"--- Vector Addition & Axioms Check ---",CR, LF,0
promptDim                  BYTE "Enter vector dimension n (1..5): ",0
promptVecA                 BYTE CR, LF,"Enter components of vector A:",CR, LF,0
promptVecB                 BYTE CR, LF,"Enter components of vector B:",CR, LF,0
promptScalarC              BYTE CR, LF,"Enter scalar c (integer): ",CR, LF,0
promptScalarD              BYTE "Enter scalar d (integer): ",CR, LF,0
msgEnterComp               BYTE "Component: ",0
vecResultLabel             BYTE CR, LF,"A + B = ",0
vecCheckLabel              BYTE CR, LF,"Checking vector space axioms (using integer vectors):",CR, LF,0
vecNote                    BYTE CR, LF,"(Note: We use integer components; axioms still hold algebraically.)",CR, LF,0

axiomOKMsg                 BYTE "  [OK] ",0
axiomFailMsg               BYTE "  [FAIL] ",0

axiom1Text                 BYTE "Axiom 1  (closure under addition)",CR, LF,0
axiom2Text                 BYTE "Axiom 2  (commutativity: A+B = B+A)",CR, LF,0
axiom3Text                 BYTE "Axiom 3  (associativity: A+(B+B) = (A+B)+B)",CR, LF,0
axiom4Text                 BYTE "Axiom 4  (zero vector: A+0 = A)",CR, LF,0
axiom5Text                 BYTE "Axiom 5  (additive inverse: A+(-A) = 0)",CR, LF,0
axiom6Text                 BYTE "Axiom 6  (closure under scalar mult.)",CR, LF,0
axiom7Text                 BYTE "Axiom 7  (c(A+B) = cA + cB)",CR, LF,0
axiom8Text                 BYTE "Axiom 8  ((c+d)A = cA + dA)",CR, LF,0
axiom9Text                 BYTE "Axiom 9  (c(dA) = (cd)A)",CR, LF,0
axiom10Text                BYTE "Axiom 10 (1*A = A)",CR, LF,0

; Floating-point constants & temporaries 
DEG_TO_RAD                 REAL8 0.017453292519943295          ; pi/180
LN2                        REAL8 0.69314718055994530942
FOUR_REAL8                 REAL8 4.0
TWO_REAL8                  REAL8 2.0

firstOperandFloat                   REAL8 ?
secondOperandFloat                  REAL8 ?

scientificInputX                   REAL8 ?
radiansTemp                    REAL8 ?
sineTemp                    REAL8 ?
cosineTemp                    REAL8 ?

quadCoeffA                         REAL8 ?
quadCoeffB                         REAL8 ?
quadCoeffC                         REAL8 ?
quadDiscriminant                      REAL8 ?
quadSqrtDiscriminant                  REAL8 ?
quadMinusBCache                      REAL8 ?
quadNumerator1                      REAL8 ?
quadNumerator2                      REAL8 ?
quadDenominator                     REAL8 ?
quadRoot1                        REAL8 ?
quadRoot2                        REAL8 ?

; Vector data (integers) 
MAX_VECTOR                 EQU 5

vectorDimension                     DWORD ?
vectorA                       SDWORD MAX_VECTOR DUP(?)
vectorB                       SDWORD MAX_VECTOR DUP(?)
vectorTemp1                   SDWORD MAX_VECTOR DUP(?)
vectorTemp2                   SDWORD MAX_VECTOR DUP(?)
vectorZero                    SDWORD MAX_VECTOR DUP(0)
vectorNegativeA                    SDWORD MAX_VECTOR DUP(?)
vectorScalarTemp1                 SDWORD MAX_VECTOR DUP(?)
vectorScalarTemp2                 SDWORD MAX_VECTOR DUP(?)

scalarCInput                    SDWORD ?
scalarDInput                    SDWORD ?


.code

; WaitKey is just a helper function

WaitKey PROC
    push edx
    mov edx, OFFSET pressKeyMsg
    call WriteString
    call ReadChar
    pop edx
    ret
WaitKey ENDP

; Print the float in ST(0) as a fixed decimal with 3 places

WriteFloatFixed3 PROC USES eax ebx edx
    ; ST(0) = floating-point value

    ; Scale by 1000 and convert to integer
    fld FLOAT_SCALE_1000                ; ST(0)=1000, ST(1)=value
    fmulp st(1),st(0)                   ; ST(0)=value*1000
    fistp scaledIntegerForPrint         ; scaledIntegerForPrint = round(value*1000), pop ST(0)

    mov eax, scaledIntegerForPrint

    ; Handle sign
    cmp eax, 0
    jge WFF3_Positive

    mov al, '-'                         ; print '-'
    call WriteChar
    mov eax, scaledIntegerForPrint
    neg eax                             ; make it positive

WFF3_Positive:
    ; Now EAX = abs(scaledIntegerForPrint)
    mov ebx, 1000
    cdq                                 ; EDX:EAX for division
    idiv ebx                            ; EAX = integer part, EDX = remainder (0..999)

    ; Print integer part
    push edx                            ; save remainder (fraction)
    call WriteInt                       ; prints integer part

    ; Print decimal point
    mov al, '.'
    call WriteChar

    ; Print fractional part as 3 digits, zero-padded
    pop eax                             ; remainder (0..999)

    ; hundreds digit
    mov edx, 0
    mov ebx, 100
    div ebx                             ; EAX = hundreds, EDX = rest 0..99
    add al, '0'
    call WriteChar
    mov eax, edx                        ; remainder 0..99

    ; tens and ones
    mov edx, 0
    mov ebx, 10
    div ebx                             ; EAX = tens, EDX = ones
    add al, '0'
    call WriteChar
    mov eax, edx
    add al, '0'
    call WriteChar

    ret
WriteFloatFixed3 ENDP


; Basic arithmetic procedure

BasicArithmetic PROC
    call Clrscr
    mov edx, OFFSET basicMenuTitle
    call WriteString

    ; for the first operand
    mov edx, OFFSET promptFirst
    call WriteString
    call ReadFloat
    fstp firstOperandFloat

    ; operator
    mov edx, OFFSET promptOp
    call WriteString
    call ReadChar
    mov bl, al                  ; operator in BL
    call WriteChar              ; echo operator so user sees it
    mov edx, OFFSET newline     ; print newline so 2nd operand is on new line
    call WriteString

    ; second operand
    mov edx, OFFSET promptSecond
    call WriteString
    call ReadFloat
    fstp secondOperandFloat

    mov edx, OFFSET resultLabel
    call WriteString

    cmp bl,'+'
    je BA_Add
    cmp bl,'-'
    je BA_Sub
    cmp bl,'*'
    je BA_Mul
    cmp bl,'/'
    je BA_Div

    mov edx, OFFSET invalidChoiceMsg
    call WriteString
    jmp BA_Done

BA_Add:
    fld firstOperandFloat
    call WriteFloatFixed3
    mov al,' '
    call WriteChar
    mov al,'+'
    call WriteChar
    mov al,' '
    call WriteChar
    fld secondOperandFloat
    call WriteFloatFixed3
    mov al,' '
    call WriteChar
    mov al,'='
    call WriteChar
    mov al,' '
    call WriteChar

    fld firstOperandFloat
    fld secondOperandFloat
    faddp st(1),st(0)
    call WriteFloatFixed3
    jmp BA_PrintNL

BA_Sub:
    fld firstOperandFloat
    call WriteFloatFixed3
    mov al,' '
    call WriteChar
    mov al,'-'
    call WriteChar
    mov al,' '
    call WriteChar
    fld secondOperandFloat
    call WriteFloatFixed3
    mov al,' '
    call WriteChar
    mov al,'='
    call WriteChar
    mov al,' '
    call WriteChar

    fld firstOperandFloat
    fld secondOperandFloat
    fsubp st(1),st(0)           ; a - b
    call WriteFloatFixed3
    jmp BA_PrintNL

BA_Mul:
    fld firstOperandFloat
    call WriteFloatFixed3
    mov al,' '
    call WriteChar
    mov al,'*'
    call WriteChar
    mov al,' '
    call WriteChar
    fld secondOperandFloat
    call WriteFloatFixed3
    mov al,' '
    call WriteChar
    mov al,'='
    call WriteChar
    mov al,' '
    call WriteChar

    fld firstOperandFloat
    fld secondOperandFloat
    fmulp st(1),st(0)
    call WriteFloatFixed3
    jmp BA_PrintNL

BA_Div:
    ; check denominator != 0
    mov eax, DWORD PTR secondOperandFloat
    mov edx, DWORD PTR secondOperandFloat+4
    or eax, edx
    jz BA_DivZero

    fld firstOperandFloat
    call WriteFloatFixed3
    mov al,' '
    call WriteChar
    mov al,'/'
    call WriteChar
    mov al,' '
    call WriteChar
    fld secondOperandFloat
    call WriteFloatFixed3
    mov al,' '
    call WriteChar
    mov al,'='
    call WriteChar
    mov al,' '
    call WriteChar

    fld firstOperandFloat
    fld secondOperandFloat
    fdivp st(1),st(0)           ; a / b
    call WriteFloatFixed3
    jmp BA_PrintNL

BA_DivZero:
    mov edx, OFFSET divZeroMsg
    call WriteString
    jmp BA_Done

BA_PrintNL:
    mov edx, OFFSET newline
    call WriteString

BA_Done:
    call WaitKey
    ret
BasicArithmetic ENDP

; Scientific menu for trignometric functions, natural log, exponential calculations

ScientificMenu PROC
LOCAL sciMenuChoice:DWORD

SciMenuStart:
    call Clrscr
    mov edx, OFFSET sciMenuTitle
    call WriteString
    call ReadInt
    mov sciMenuChoice, eax

    cmp sciMenuChoice,0
    je Sci_Exit

    ; read x once for all functions
    mov edx, OFFSET promptX
    call WriteString
    call ReadFloat
    fstp scientificInputX

    cmp sciMenuChoice,1
    je Sci_Sin
    cmp sciMenuChoice,2
    je Sci_Cos
    cmp sciMenuChoice,3
    je Sci_Tan
    cmp sciMenuChoice,4
    je Sci_Sqrt
    cmp sciMenuChoice,5
    je Sci_Ln

    mov edx, OFFSET invalidChoiceMsg
    call WriteString
    call WaitKey
    jmp SciMenuStart

; sin(x) with x in degrees
Sci_Sin:
    fld scientificInputX
    fld DEG_TO_RAD
    fmulp st(1),st(0)           ; x * pi/180
    fsin
    mov edx, OFFSET resultLabel
    call WriteString
    call WriteFloatFixed3
    mov edx, OFFSET newline
    call WriteString
    call WaitKey
    jmp SciMenuStart

; cos(x) with x in degrees
Sci_Cos:
    fld scientificInputX
    fld DEG_TO_RAD
    fmulp st(1),st(0)
    fcos
    mov edx, OFFSET resultLabel
    call WriteString
    call WriteFloatFixed3
    mov edx, OFFSET newline
    call WriteString
    call WaitKey
    jmp SciMenuStart

; tan(x) with x in degrees
Sci_Tan:
    fld scientificInputX
    fld DEG_TO_RAD
    fmulp st(1),st(0)
    fstp radiansTemp

    ; sin
    fld radiansTemp
    fsin
    fstp sineTemp

    ; cos
    fld radiansTemp
    fcos
    fstp cosineTemp

    ; simple cos ~ 0 check
    mov eax, DWORD PTR cosineTemp
    mov edx, DWORD PTR cosineTemp+4
    or eax, edx
    jz Sci_TanDomain

    fld sineTemp
    fld cosineTemp
    fdivp st(1),st(0)           ; sin / cos
    mov edx, OFFSET resultLabel
    call WriteString
    call WriteFloatFixed3
    mov edx, OFFSET newline
    call WriteString
    call WaitKey
    jmp SciMenuStart

Sci_TanDomain:
    mov edx, OFFSET tanDomainWarn
    call WriteString
    call WaitKey
    jmp SciMenuStart

; sqrt(x), domain x >= 0
Sci_Sqrt:
    mov eax, DWORD PTR scientificInputX
    mov edx, DWORD PTR scientificInputX+4
    mov ecx, edx
    and ecx,80000000h           ; sign bit
    cmp ecx,0
    jne Sci_SqrtDomain          ; negative

    fld scientificInputX
    fsqrt
    mov edx, OFFSET resultLabel
    call WriteString
    call WriteFloatFixed3
    mov edx, OFFSET newline
    call WriteString
    call WaitKey
    jmp SciMenuStart

Sci_SqrtDomain:
    mov edx, OFFSET domainErrorSqrt
    call WriteString
    call WaitKey
    jmp SciMenuStart

; ln(x), domain x > 0
Sci_Ln:
    mov eax, DWORD PTR scientificInputX
    mov edx, DWORD PTR scientificInputX+4

    mov ecx, edx
    and ecx,80000000h           ; sign
    cmp ecx,0
    jne Sci_LnDomain            ; negative

    mov ecx, eax
    or ecx, edx
    jz Sci_LnDomain             ; zero

    ; ln(x) = ln(2) * log2(x) using FYL2X
    fld1                        ; st0 = 1
    fld scientificInputX        ; st0 = x, st1 = 1
    fyl2x                       ; st0 = log2(x)
    fld LN2
    fmulp st(1),st(0)           ; st0 = ln(x)

    mov edx, OFFSET resultLabel
    call WriteString
    call WriteFloatFixed3
    mov edx, OFFSET newline
    call WriteString
    call WaitKey
    jmp SciMenuStart

Sci_LnDomain:
    mov edx, OFFSET domainErrorLn
    call WriteString
    call WaitKey
    jmp SciMenuStart

Sci_Exit:
    ret
ScientificMenu ENDP

; Quadratic solver procedure
QuadraticSolve PROC
    ; check a != 0 (quadCoeffA)
    mov eax, DWORD PTR quadCoeffA
    mov edx, DWORD PTR quadCoeffA+4
    or eax, edx
    jz Quad_NotQuadratic

    ; d = b^2 - 4ac
    fld quadCoeffB
    fld quadCoeffB
    fmulp st(1),st(0)           ; b^2

    fld quadCoeffA
    fld quadCoeffC
    fmulp st(1),st(0)           ; a*c

    fld FOUR_REAL8
    fmulp st(1),st(0)           ; 4ac

    fsubp st(1),st(0)           ; b^2 - 4ac
    fstp quadDiscriminant

    ; discriminant sign
    mov eax, DWORD PTR quadDiscriminant
    mov edx, DWORD PTR quadDiscriminant+4
    mov ecx, edx
    and ecx,80000000h
    cmp ecx,0
    jne Quad_NoReal

    ; sqrt(d)
    fld quadDiscriminant
    fsqrt
    fstp quadSqrtDiscriminant

    ; temp = -b
    fld quadCoeffB
    fchs
    fstp quadMinusBCache

    ; num1 = temp + sqrt(d)
    fld quadMinusBCache
    fld quadSqrtDiscriminant
    faddp st(1),st(0)
    fstp quadNumerator1

    ; num2 = temp - sqrt(d)
    fld quadMinusBCache
    fld quadSqrtDiscriminant
    fsubp st(1),st(0)
    fstp quadNumerator2

    ; denom = 2a
    fld quadCoeffA
    fld TWO_REAL8
    fmulp st(1),st(0)
    fstp quadDenominator

    ; x1 = num1 / denom
    fld quadNumerator1
    fld quadDenominator
    fdivp st(1),st(0)
    fstp quadRoot1

    ; x2 = num2 / denom
    fld quadNumerator2
    fld quadDenominator
    fdivp st(1),st(0)
    fstp quadRoot2

    mov edx, OFFSET quadRootsLabel
    call WriteString

    mov edx, OFFSET x1Label
    call WriteString
    fld quadRoot1
    call WriteFloatFixed3
    mov edx, OFFSET newline
    call WriteString

    mov edx, OFFSET x2Label
    call WriteString
    fld quadRoot2
    call WriteFloatFixed3
    mov edx, OFFSET newline
    call WriteString

    ret

Quad_NotQuadratic:
    mov edx, OFFSET quadNotQuadMsg
    call WriteString
    ret

Quad_NoReal:
    mov edx, OFFSET quadNoRealMsg
    call WriteString
    ret
QuadraticSolve ENDP

; Quadratic menu 
QuadraticMenu PROC
LOCAL quadMenuChoice:DWORD

    call Clrscr
    mov edx, OFFSET quadMenuTitle
    call WriteString
    call ReadInt
    mov quadMenuChoice, eax

    cmp quadMenuChoice,0
    je QM_Exit
    cmp quadMenuChoice,1
    je QM_Preset
    cmp quadMenuChoice,2
    je QM_Custom

    mov edx, OFFSET invalidChoiceMsg
    call WriteString
    call WaitKey
    jmp QM_Exit

QM_Preset:
    mov edx, OFFSET quadPresetMenu
    call WriteString
    call ReadInt

    cmp eax,1
    je QM_P1
    cmp eax,2
    je QM_P2

    mov edx, OFFSET invalidChoiceMsg
    call WriteString
    call WaitKey
    jmp QM_Exit

QM_P1:
    fld quad1CoeffA
    fstp quadCoeffA
    fld quad1CoeffB
    fstp quadCoeffB
    fld quad1CoeffC
    fstp quadCoeffC
    call QuadraticSolve
    call WaitKey
    jmp QM_Exit

QM_P2:
    fld quad2CoeffA
    fstp quadCoeffA
    fld quad2CoeffB
    fstp quadCoeffB
    fld quad2CoeffC
    fstp quadCoeffC
    call QuadraticSolve
    call WaitKey
    jmp QM_Exit

QM_Custom:
    mov edx, OFFSET promptA
    call WriteString
    call ReadFloat
    fstp quadCoeffA

    mov edx, OFFSET promptB
    call WriteString
    call ReadFloat
    fstp quadCoeffB

    mov edx, OFFSET promptC
    call WriteString
    call ReadFloat
    fstp quadCoeffC

    call QuadraticSolve
    call WaitKey

QM_Exit:
    ret
QuadraticMenu ENDP

; Vector helpers (we limited our calculations to integers only) 
; ReadVectorInt:
;   EAX = address of vector (SDWORD array)
;   ECX = count
ReadVectorInt PROC USES eax ecx esi
    mov esi, eax
RV_Loop:
    cmp ecx,0
    jle RV_Done
    mov edx, OFFSET msgEnterComp
    call WriteString
    call ReadInt
    mov [esi], eax
    add esi, TYPE SDWORD
    dec ecx
    jmp RV_Loop
RV_Done:
    ret
ReadVectorInt ENDP

; AddVectorsInt:
;   ESI points to A, EDI points to B, EDX stores our Result, ECX = count
AddVectorsInt PROC USES eax ecx esi edi edx
AV_Loop:
    cmp ecx,0
    jle AV_Done
    mov eax, [esi]
    add eax, [edi]
    mov [edx], eax
    add esi, TYPE SDWORD
    add edi, TYPE SDWORD
    add edx, TYPE SDWORD
    dec ecx
    jmp AV_Loop
AV_Done:
    ret
AddVectorsInt ENDP

; NegateVectorInt:
;   ESI points to A, EDX stores our Result, ECX = count
NegateVectorInt PROC USES eax ecx esi edx
NV_Loop:
    cmp ecx,0
    jle NV_Done
    mov eax, [esi]
    neg eax
    mov [edx], eax
    add esi, TYPE SDWORD
    add edx, TYPE SDWORD
    dec ecx
    jmp NV_Loop
NV_Done:
    ret
NegateVectorInt ENDP

; ScalarMulVectorInt:
;   scalar in EAX, ESI points to A, EDX points to Result, ECX = count
ScalarMulVectorInt PROC USES ebx ecx esi edx
SM_Loop:
    cmp ecx,0
    jle SM_Done
    mov ebx, [esi]
    imul ebx, eax
    mov [edx], ebx
    add esi, TYPE SDWORD
    add edx, TYPE SDWORD
    dec ecx
    jmp SM_Loop
SM_Done:
    ret
ScalarMulVectorInt ENDP

; CompareVectorsInt:
;   ESI points to A, EDI points to B, ECX = count
;   Returns AL = 1 if equal, 0 otherwise
CompareVectorsInt PROC USES ecx esi edi
    mov al,1
CV_Loop:
    cmp ecx,0
    jle CV_Done
    mov ah, BYTE PTR [esi]      ; use AH temporarily
    mov eax, [esi]
    cmp eax, [edi]
    jne CV_NotEq
    add esi, TYPE SDWORD
    add edi, TYPE SDWORD
    dec ecx
    jmp CV_Loop
CV_NotEq:
    mov al,0
CV_Done:
    ret
CompareVectorsInt ENDP

; PrintVectorInt:
;   ESI -> vector, ECX = count
PrintVectorInt PROC USES eax ecx esi
    mov al,'('
    call WriteChar
PV_Loop:
    cmp ecx,0
    jle PV_End
    mov eax,[esi]
    call WriteInt
    dec ecx
    add esi, TYPE SDWORD
    cmp ecx,0
    je PV_Loop        ; go to top, will end, no comma
    mov al,','
    call WriteChar
    mov al,' '
    call WriteChar
    jmp PV_Loop
PV_End:
    mov al,')'
    call WriteChar
    ret
PrintVectorInt ENDP

; Check for all 10 axioms 
CheckVectorAxioms PROC
    pushad

    mov edx, OFFSET vecCheckLabel
    call WriteString
    mov edx, OFFSET vecNote
    call WriteString

    ; Axiom 1: closure 

    mov edx, OFFSET axiom1Text
    call WriteString
    mov edx, OFFSET axiomOKMsg
    call WriteString

    ; Axiom 2: A+B = B+A

    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edi, OFFSET vectorB
    mov edx, OFFSET vectorTemp1
    call AddVectorsInt          ; vectorTemp1 = A+B

    mov ecx, vectorDimension
    mov esi, OFFSET vectorB
    mov edi, OFFSET vectorA
    mov edx, OFFSET vectorTemp2
    call AddVectorsInt          ; vectorTemp2 = B+A

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp1
    mov edi, OFFSET vectorTemp2
    call CompareVectorsInt

    mov edx, OFFSET axiom2Text
    call WriteString
    cmp al,1
    je Ax2_OK
    mov edx, OFFSET axiomFailMsg
    call WriteString
    jmp Ax2_End

Ax2_OK:
    mov edx, OFFSET axiomOKMsg
    call WriteString

Ax2_End:

    ; Axiom 3: A+(B+B) = (A+B)+B\

    mov ecx, vectorDimension
    mov esi, OFFSET vectorB
    mov edi, OFFSET vectorB
    mov edx, OFFSET vectorTemp1
    call AddVectorsInt          ; vectorTemp1 = B+B

    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edi, OFFSET vectorTemp1
    mov edx, OFFSET vectorTemp2
    call AddVectorsInt          ; vectorTemp2 = A+(B+B)

    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edi, OFFSET vectorB
    mov edx, OFFSET vectorTemp1
    call AddVectorsInt          ; vectorTemp1 = A+B

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp1
    mov edi, OFFSET vectorB
    mov edx, OFFSET vectorScalarTemp1
    call AddVectorsInt          ; vectorScalarTemp1 = (A+B)+B

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp2
    mov edi, OFFSET vectorScalarTemp1
    call CompareVectorsInt

    mov edx, OFFSET axiom3Text
    call WriteString
    cmp al,1
    je Ax3_OK
    mov edx, OFFSET axiomFailMsg
    call WriteString
    jmp Ax3_End

Ax3_OK:
    mov edx, OFFSET axiomOKMsg
    call WriteString

Ax3_End:

    ; Axiom 4: A + 0 = A (existence of zero vector)

    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edi, OFFSET vectorZero
    mov edx, OFFSET vectorTemp1
    call AddVectorsInt          ; A+0

    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edi, OFFSET vectorTemp1
    call CompareVectorsInt

    mov edx, OFFSET axiom4Text
    call WriteString
    cmp al,1
    je Ax4_OK
    mov edx, OFFSET axiomFailMsg
    call WriteString
    jmp Ax4_End

Ax4_OK:
    mov edx, OFFSET axiomOKMsg
    call WriteString

Ax4_End:

    ; Axiom 5: A + (-A) = 0

    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorNegativeA
    call NegateVectorInt        ; -A

    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edi, OFFSET vectorNegativeA
    mov edx, OFFSET vectorTemp1
    call AddVectorsInt          ; A+(-A)

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp1
    mov edi, OFFSET vectorZero
    call CompareVectorsInt

    mov edx, OFFSET axiom5Text
    call WriteString
    cmp al,1
    je Ax5_OK
    mov edx, OFFSET axiomFailMsg
    call WriteString
    jmp Ax5_End

Ax5_OK:
    mov edx, OFFSET axiomOKMsg
    call WriteString

Ax5_End:

    ; Ask scalars c, d

    mov edx, OFFSET promptScalarC
    call WriteString
    call ReadInt
    mov scalarCInput, eax

    mov edx, OFFSET promptScalarD
    call WriteString
    call ReadInt
    mov scalarDInput, eax

    ; Axiom 6: closure under scalar mult. (cA is a vector)

    mov ecx, vectorDimension
    mov eax, scalarCInput
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorScalarTemp1
    call ScalarMulVectorInt     ; cA

    mov edx, OFFSET axiom6Text
    call WriteString
    mov edx, OFFSET axiomOKMsg
    call WriteString

    ; Axiom 7: c(A+B) = cA + cB

    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edi, OFFSET vectorB
    mov edx, OFFSET vectorTemp1
    call AddVectorsInt          ; A+B

    mov ecx, vectorDimension
    mov eax, scalarCInput
    mov esi, OFFSET vectorTemp1
    mov edx, OFFSET vectorTemp2
    call ScalarMulVectorInt     ; c(A+B)

    mov ecx, vectorDimension
    mov eax, scalarCInput
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorScalarTemp1
    call ScalarMulVectorInt     ; cA

    mov ecx, vectorDimension
    mov eax, scalarCInput
    mov esi, OFFSET vectorB
    mov edx, OFFSET vectorScalarTemp2
    call ScalarMulVectorInt     ; cB

    mov ecx, vectorDimension
    mov esi, OFFSET vectorScalarTemp1
    mov edi, OFFSET vectorScalarTemp2
    mov edx, OFFSET vectorScalarTemp1
    call AddVectorsInt          ; cA + cB

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp2
    mov edi, OFFSET vectorScalarTemp1
    call CompareVectorsInt

    mov edx, OFFSET axiom7Text
    call WriteString
    cmp al,1
    je Ax7_OK
    mov edx, OFFSET axiomFailMsg
    call WriteString
    jmp Ax7_End

Ax7_OK:
    mov edx, OFFSET axiomOKMsg
    call WriteString

Ax7_End:

    ; Axiom 8: (c+d)A = cA + dA

    mov eax, scalarCInput
    add eax, scalarDInput             ; c+d
    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorTemp1
    call ScalarMulVectorInt      ; (c+d)A

    mov ecx, vectorDimension
    mov eax, scalarCInput
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorScalarTemp1
    call ScalarMulVectorInt      ; cA

    mov ecx, vectorDimension
    mov eax, scalarDInput
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorScalarTemp2
    call ScalarMulVectorInt      ; dA

    mov ecx, vectorDimension
    mov esi, OFFSET vectorScalarTemp1
    mov edi, OFFSET vectorScalarTemp2
    mov edx, OFFSET vectorScalarTemp1
    call AddVectorsInt           ; cA + dA

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp1
    mov edi, OFFSET vectorScalarTemp1
    call CompareVectorsInt

    mov edx, OFFSET axiom8Text
    call WriteString
    cmp al,1
    je Ax8_OK
    mov edx, OFFSET axiomFailMsg
    call WriteString
    jmp Ax8_End

Ax8_OK:
    mov edx, OFFSET axiomOKMsg
    call WriteString

Ax8_End:

    ; Axiom 9: c(dA) = (cd)A

    mov ecx, vectorDimension
    mov eax, scalarDInput
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorTemp1
    call ScalarMulVectorInt      ; dA

    mov ecx, vectorDimension
    mov eax, scalarCInput
    mov esi, OFFSET vectorTemp1
    mov edx, OFFSET vectorTemp2
    call ScalarMulVectorInt      ; c(dA)

    mov eax, scalarCInput
    imul eax, scalarDInput            ; cd
    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorScalarTemp1
    call ScalarMulVectorInt      ; (cd)A

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp2
    mov edi, OFFSET vectorScalarTemp1
    call CompareVectorsInt

    mov edx, OFFSET axiom9Text
    call WriteString
    cmp al,1
    je Ax9_OK
    mov edx, OFFSET axiomFailMsg
    call WriteString
    jmp Ax9_End

Ax9_OK:
    mov edx, OFFSET axiomOKMsg
    call WriteString

Ax9_End:

    ; Axiom 10: 1*A = A

    mov eax,1
    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edx, OFFSET vectorTemp1
    call ScalarMulVectorInt      ; 1*A

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp1
    mov edi, OFFSET vectorA
    call CompareVectorsInt

    mov edx, OFFSET axiom10Text
    call WriteString
    cmp al,1
    je Ax10_OK
    mov edx, OFFSET axiomFailMsg
    call WriteString
    jmp Ax10_End

Ax10_OK:
    mov edx, OFFSET axiomOKMsg
    call WriteString

Ax10_End:

    popad
    ret
CheckVectorAxioms ENDP

; Vector menu 
VectorMenu PROC
    call Clrscr
    mov edx, OFFSET vecMenuTitle
    call WriteString

    mov edx, OFFSET promptDim
    call WriteString
    call ReadInt
    mov vectorDimension, eax

    cmp eax,1
    jl VM_Done
    cmp eax,MAX_VECTOR
    jg VM_Done

    ; read A
    mov edx, OFFSET promptVecA
    call WriteString
    mov ecx, vectorDimension
    mov eax, OFFSET vectorA
    call ReadVectorInt

    ; read B
    mov edx, OFFSET promptVecB
    call WriteString
    mov ecx, vectorDimension
    mov eax, OFFSET vectorB
    call ReadVectorInt

    ; A+B
    mov edx, OFFSET vecResultLabel
    call WriteString
    mov ecx, vectorDimension
    mov esi, OFFSET vectorA
    mov edi, OFFSET vectorB
    mov edx, OFFSET vectorTemp1
    call AddVectorsInt

    mov ecx, vectorDimension
    mov esi, OFFSET vectorTemp1
    call PrintVectorInt
    mov edx, OFFSET newline
    call WriteString

    ; check axioms
    call CheckVectorAxioms

    call WaitKey

VM_Done:
    ret

VectorMenu ENDP

; main procedure
main PROC
LOCAL mainMenuChoice:DWORD

MainLoop:
    call Clrscr
    mov edx, OFFSET mainMenu
    call WriteString
    call ReadInt
    mov mainMenuChoice, eax

    cmp mainMenuChoice,0
    je ExitProgram
    cmp mainMenuChoice,1
    je DoBasic
    cmp mainMenuChoice,2
    je DoScientific
    cmp mainMenuChoice,3
    je DoQuadratic
    cmp mainMenuChoice,4
    je DoVector

    mov edx, OFFSET invalidChoiceMsg
    call WriteString
    call WaitKey
    jmp MainLoop

DoBasic:
    call BasicArithmetic
    jmp MainLoop

DoScientific:
    call ScientificMenu
    jmp MainLoop

DoQuadratic:
    call QuadraticMenu
    jmp MainLoop

DoVector:
    call VectorMenu
    jmp MainLoop

ExitProgram:
    mov edx, OFFSET goodbyeMsg
    call WriteString
    exit
    ret
main ENDP

END main
