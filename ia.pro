<?xml version="1.0"?>

<chapter>

<title>AdaOS Native Ada Compiler</title>

<subtitle>Intel Architecture (IA): Basic Level (equivalent to iAPX
386)</subtitle>

<para>Timings and encodings are made with the assumption the processor is
executing in 32-bit operand/address mode.</para>





<synopsis>
instruc(ia,I,E)
</synopsis>

<para>Succeeds if I is a valid IA machine instruction which has encoding E
(a list of byte values 0-255).</para>

<para>ASCII adjust for divide:</para>

+++

instruc(ia,'AAD',[213,10]).

---

<para>Add:</para>

+++

instruc(ia,'ADD'(Rd,Rs),[0,E2]) :-
   reg(ia,Rd,sint,8,Ed),
   reg(ia,Rs,sint,8,Es),
   E2 is 3*64+Ed*8+Es.

instruc(ia,'ADD'(Rd,Rs),[0,E2]) :-
   reg(ia,Rd,uint,8,Ed),
   reg(ia,Rs,uint,8,Es),
   E2 is 3*64+Ed*8+Es.

instruc(ia,'ADD'(Rd,Rs),???,[1,E2]) :-
   reg(ia,Rd,sint,32,Ed),
   reg(ia,Rs,sint,32,Es),
   E2 is 3*64+Ed*8+Es.

instruc(ia,'ADD'(Rd,Rs),[1,E2]) :-
   reg(ia,Rd,uint,32,Ed),
   reg(ia,Rs,uint,32,Es),
   E2 is 3*64+Ed*8+Es.

??? etc

---



<synopsis>
reg(ia,R,T,E)
</synopsis>

<para>Succeeds if R is a general-purpose register of type T (which includes
the length in bits) and instruction encoding E.</para>

+++

reg(ia,'AL',sint(8),0).
reg(ia,'CL',sint(8),1).
reg(ia,'DL',sint(8),2).
reg(ia,'BL',sint(8),3).
reg(ia,'AH',sint(8),4).
reg(ia,'CH',sint(8),5).
reg(ia,'DH',sint(8),6).
reg(ia,'BH',sint(8),7).

reg(ia,'AX',sint(16),0).
reg(ia,'CX',sint(16),1).
reg(ia,'DX',sint(16),2).
reg(ia,'BX',sint(16),3).
reg(ia,'SP',sint(16),4).
reg(ia,'BP',sint(16),5).
reg(ia,'SI',sint(16),6).
reg(ia,'DI',sint(16),7).

reg(ia,'EAX',sint(32),0).
reg(ia,'ECX',sint(32),1).
reg(ia,'EDX',sint(32),2).
reg(ia,'EBX',sint(32),3).
reg(ia,'ESP',sint(32),4).
reg(ia,'EBP',sint(32),5).
reg(ia,'ESI',sint(32),6).
reg(ia,'EDI',sint(32),7).

reg(ia,'AL',uint(8),0).
reg(ia,'CL',uint(8),1).
reg(ia,'DL',uint(8),2).
reg(ia,'BL',uint(8),3).
reg(ia,'AH',uint(8),4).
reg(ia,'CH',uint(8),5).
reg(ia,'DH',uint(8),6).
reg(ia,'BH',uint(8),7).

reg(ia,'AX',uint(16),0).
reg(ia,'CX',uint(16),1).
reg(ia,'DX',uint(16),2).
reg(ia,'BX',uint(16),3).
reg(ia,'SP',uint(16),4).
reg(ia,'BP',uint(16),5).
reg(ia,'SI',uint(16),6).
reg(ia,'DI',uint(16),7).

reg(ia,'EAX',uint(32),0).
reg(ia,'ECX',uint(32),1).
reg(ia,'EDX',uint(32),2).
reg(ia,'EBX',uint(32),3).
reg(ia,'ESP',uint(32),4).
reg(ia,'EBP',uint(32),5).
reg(ia,'ESI',uint(32),6).
reg(ia,'EDI',uint(32),7).

reg(ia,'ST0',flt(80),0). % top of NDP float stack
reg(ia,'ST1',flt(80),1). % top of stack -1
reg(ia,'ST2',flt(80),2). % TOS-2
reg(ia,'ST3',flt(80),3). % TOS-3
reg(ia,'ST4',flt(80),4). % TOS-4
reg(ia,'ST5',flt(80),5). % TOS-5
reg(ia,'ST6',flt(80),6). % TOS-6
reg(ia,'ST7',flt(80),7). % TOS-7


% ---------------------------------------------------------
% overlaps(R1,R2,D)

% Succeeds if register R1 overlaps register R2, where R1 (its bit 0) starts
% at bit D in R2. By convention, R1 will always be smaller than R2.

overlaps('AL','AX',0).
overlaps('AH','AX',8).
overlaps('BL','BX',0).
overlaps('BH','BX',8).
overlaps('CL','CX',0).
overlaps('CH','CX',8).
overlaps('DL','DX',0).
overlaps('DH','DX',8).

overlaps('AX','EAX',0).
overlaps('BX','EBX',0).
overlaps('CX','ECX',0).
overlaps('DX','EDX',0).
overlaps('SI','ESI',0).
overlaps('DI','EDI',0).
overlaps('BP','EBP',0).
overlaps('SP','ESP',0).

overlaps('AL','EAX',0).
overlaps('AH','EAX',8).
overlaps('BL','EBX',0).
overlaps('BH','EBX',8).
overlaps('CL','ECX',0).
overlaps('CH','ECX',8).
overlaps('DL','EDX',0).
overlaps('DH','EDX',8).



% ---------------------------------------------------------
% ia_seg_prefix(S,P)

%

ia_seg_prefix('CS',46).
ia_seg_prefix('SS',54).
ia_seg_prefix('DS',62).
ia_seg_prefix('ES',38).
ia_seg_prefix('FS',100).
ia_seg_prefix('GS',101).


% ---------------------------------------------------------
% ia_default_seg(R,S)

%

ia_default_seg('ESP','SS').
ia_default_seg('EBP','SS').
ia_default_seg(R,'DS') :- R \= 'ESP', R \= 'EBP'.


% ---------------------------------------------------------
% ia_operand(M,T,E,P)

% Succeeds if M is a valid memory/register addressing mode for an operand
% of type (and length) T whose encoding is E with prefix P (both byte
% lists).

ia_op_seg_prefix(R,S,[]) :- ia_default_seg(R,S).
ia_op_seg_prefix(R,S,[V]) :- not ia_default_seg(R,S), ia_seg_prefix(S,V).

ia_op_disp(D,1,[V]) :- between(D,-128,-1), V is D+256.
ia_op_disp(D,1,[D]) :- between(D,0,127).
ia_op_disp(D,2,[V1,V2,V3]) :- D >= 128, V1 is D\256, V2 is (D/256)\256, V3 is D/256/256.
ia_op_disp(D,2,[V1,V2,V3]) :- D < 128, N is -D, V1 is 256-N\256) V2 is 256-(/256)\256, V3 is 256-D/256/256.

ia_operand(R,T,0,[3*64+E*8+0],[]) :- reg(ia,R,T,E).

ia_operand(mem(S,R+D),_,[M*64+RE*8+0,|DE],P) :-
   reg(ia,R,sint(32),RE),
   R \= 'ESP',
   ia_op_seg_prefix(R,S,P),
   ia_op_disp(D,M,DE)
   ...

ia_operand(
, R \= 'ESP'
























% ---------------------------------------------------------
% compile_i(A,M,I,P1,P2,T,E)

% Succeeds if intermediate instruction M can be transformed into (has the
% same effect as) assembly code I (a list of instructions) belonging to
% machine architecture A, where P1 is the contextual (preceding) register
% profile (a tree), and P2 is the resulting register profile, T is the
% total number of clocks cycles taken to execute, and E is the resulting
% encoding (a list of byte values).


compile_i(ia,(Targ := X),I,P1,P3,T3,E3) :-
   compile_e(ia,X,I1,P1,P2,T1,E1,R),
   ...



% ---------------------------------------------------------
% compile_e(A,X,I,P1,P2,T,E,R)

% Succeeds if intermediate expression X can be transformed into (has the
% same effect as) assembly code I (a list of instructions) belonging to
% machine architecture A, where P1 is the contextual (preceding) register
% profile (a tree), and P2 is the resulting register profile, T is the
% total number of clocks cycles taken to execute, E is the resulting
% encoding (a list of byte values), and R is the register containing the
% resulting value.


% If the expression is already in a register, there is nothing to do:

compile_e(_,X,P,P,0,[],R) :- tree_member(P,R,X), !.


% Otherwise, if the 'expression' is an object, we must load it into a
% register:

compile_e(ia,X,P1,P2,T,E,R) :-
   ...


%

compile_e(ia,X1+X2,I4,P1,P4,T4,E4,R1) :-
   compile_e(ia,X1,I1,P1,P2,T1,E1,R1),
   compile_e(ia,X2,I2,P2,P3,T2,E2,R2),
   instruc(ia,'ADD'(R1,R2),T3,E3),
   tree_modify(P3,R1,X1+X2,P4),
   T4 is T1+T2+T3,
   list_append(I1,I2,It), list_append(It,['ADD'(R1,R2)],I4),
   list_append(E1,E2,Et), list_append(Et,E3,E4).
   




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% 

% 




% ---------------------------------------------------------
% End of File.

