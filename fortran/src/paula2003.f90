! ======================================================================
! Comments by Paula Coelho in 2003
! ======================================================================
!     >> pfantgrade.f << NOV 2003
!
!     este codigo eh uma uniao do codigo pfant03.f e do
!     pfant01h.f feita pela Paula Coelho e Jorge Melendez. O
!     codigo pfant03.f calculava apenas 4 linhas de hidrogenio enquanto
!     que pfant01.h (MNoel) calculava 10 linhas.
!     Esta relatado a seguir as modificacoes feitas ao pfant03.f
!     para chegar ao presente codigo.
!
!     Passos realizados (nov/2003):
!     1) copiei todos os fontes que estavam em /home1/barbuy/pfant03
!
!     2) troquei nome do arquivo de atomos para 'dfile:atoms' e o de
!     moleculas para 'dfile:molecules'
!
!     3) examinei os codigos pabsor.f pcalr98bht.f, pncalr98.f e
!     psatox95t.f e retirei as rotinas que eram obsoletas e nao eram
!     mais utilizadas (rotinas RETIRADAS => pcalr98bht.f: cafconvh,
!     voluteh / pncalr98.f: bkf, ediga, equiv, flin2, flin2b, inait,
!     inaitb, largq, popul2, quid, selekf, trangx, step, stepb,
!     volute, naitk3, gam, xxsol.
!
!     4) juntei os arquivos limpos conforme item anterior pabsor.f,
!     pcalr98bht.f, pfant03.f, pncalr98.f psatox95t.f em um
!     UNICO FONTE PFANTGRADE.F. Dessa forma, apenas o pkapgeralgrade.f
!     continua sendo um arquivo externo, devido a sua atualizacao ser
!     SEMPRE paralela com alteracoes no arquivo de moleculas.
!
!     Portanto, a nova forma de compilar o programa eh
!     f90 -o pfantgrade pfantgrade.f pkapgeralgrade.f
!
!     5) Aumentei o tamanho maximo do espectro total possivel de
!     ser calculado de 10000A para 20000A.
!
!     6) AQUI COMECAM AS ALTERACOES DEVIDO AO CALCULO DAS LINHAS DE H
!
!     a. linhas de codigo foram comentadas (identificadas com 'cp Nov03':
!     (comentario da Paula em Nov03).
!
!     b. dimensao e data de filetoh_llhy e dimensao de main_FILETOHY foram
!     atualizadas
!
!     c. incluidos ct_tauhi(MAX_DTOT,50),TAUHY(10,MAX_DTOT,50) e excluido IHH(500)
!
!     d. todo o codigo que se referia ao calculo das linha de H foram
!     ocultados, e o codigo a isto referente que estava em pfant01.h
!     foi acrescentado (correspondo ao codigo na secao
!     LECTURE TAU RAIE HYDROGENE ET INTERPOLATION DE TAUH ).
!
!     e. segundo as instrucoes enviadas pela Marie Noel em 2001:
!     - na rotina FTLIN3H foi incluida a linha
!     'if (ftt(itot).ne.0.0) k2=itot'
!     - DTOT foi substituido por MAX_DTOT nas dimensoes das matrizes
!           BK : TTD(MAX_DTOT), bk_KCD(MAX_DTOT,5)
!        LECTAUH : TTD(MAX_DTOT)
!        SELEKFH : TTD(MAX_DTOT), bk_KCD(MAX_DTOT, 50), selekfh_FL(MAX_DTOT), TAUH(MAX_DTOT,50)`
!
!
!     Tambem reduzi o numero de comentario que vao para a tela
!     'write(6...)' := cpc
!
! ========================================================================
!
!     Alteracao para calcular simultaneamente o continuo selekfh_FCONT(MAX_DTOT) e o
!     espectro normalizado FN(MAX_DTOT) {Paula, dez 2003}
!
!     - acrescentei as variaveis FN, selekfh_FCONT, fn_flux2, fn_flux3
!     - abro mais dois arquivos binarios unit=UNIT_CONT (continuo) e UNIT_NORM (normalizado)
!     - rotina SELEKFH:
!           - recebe tbem selekfh_FCONT e FN
!           - selekfh_FCONT eh calculado passando p/ a rotina FLIN1 apenas o
!           coeficiente de absorcao do continuo
!     - FN = selekfh_FL / selekfh_FCONT
!     - escrevo nos devidos arquivos
!
!     Portanto, para diferenciar os arquivos binarios criados,
!     alem do arquivo normal criado como 'spe.' + nome no dfile:main
!     o pfant cria mais dois arquivos que comecam com 'cont.' e 'norm.'
!     [edit: pfant output files are text files, not binary]
!===PC2003 END===
