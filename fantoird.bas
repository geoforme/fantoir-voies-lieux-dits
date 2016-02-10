'programme :"fantoird" pour defragmenter le fichier fantoir par departement
'**************************************************************************
'
'derniere mise a jour : 28 janvier 2016
'
'
'version$="2016"
'
'
'choix de l'extension des fichiers a creer
'*****************************************
'
'
extension$=".txt"
'
'traitement des erreurs
'**********************
'on error goto erreurs
'
'deroulement du programme
'************************
'
'
'initialisation des disques, r‚pertoires et noms de fichiers utilis‚s
'********************************************************************
'
ficlect$="C:\FANTOIRD" rem D comme Decompresse "C:\FANTOIR.txt"
'
'
disqueecrit$="C:\"
'
'
debutnom$="lieu-"
test$="01"
'
'
'lecture du fichier fantoir.txt et defragmentation par departement
'*****************************************************************
open ficlect$ for input AS #1
line input #1,debut$ 'ligne contenant entre autre information la date de mise a jour du fichier FANTOIR
debut$=mid$(debut$,37,8)
line input #1,departement$ ' ligne avec nom du departement
departement$=test$+"         "+mid$(departement$,12,30)
'
cls
'
'
nbl=1
nblt=0
nbcom=0
nbcomtotal=0
testd3=1
'
'
ficecrit$=disqueecrit$+debutnom$+test$+extension$
ficcommunes$=disqueecrit$+"communes"+extension$
'locate 4,1  ' attention 8 caracteres maxi pour le nom du fichier creer
'print ficecrit$
open ficecrit$ for output AS #2
open ficcommunes$ for output AS #3
print #2,debut$
print #2,departement$
'
'
locate 3,1
print "dpt"
locate 3,10
print "en cours de traitement..."
dpt$="010"
test$="010"
np=0
l=0
m=0
n=0
p=0
'
while not eof(1)
line input #1,ligne$
dptesp$=mid$(ligne$,4,8)
dpt$=left$(ligne$,3)
pc$=left$(ligne$,2)
'
if right$(dpt$,1)="0" then
                          dptecrit$=left$(dpt$,2)
                      else
                          if pc$="13" then
                                          dpt$="130"
                                          dptecrit$="13"
                          end if
                          if pc$="59" then
                                          dpt$="590"
                                          dptecrit$="59"
                          end if
                          if pc$="75" then
                                          dpt$="750"
                                          dptecrit$="75"
                          end if
                          if pc$="92" then
                                          dpt$="920"
                                          dptecrit$="92"
                          end if
                          if pc$>"95" then
                                          dptecrit$=dpt$
                          end if
end if
'
'
if dpt$=test$ then
                  locate 3,5
                  print space$(3)
                  locate 3,5
                  print dptecrit$
                  nbl=nbl+1
                  nblt=nblt+1
                  blanc$=mid$(ligne$,7,1)
                  if blanc$="B" then
                                    ec$="LIEU-DIT   "
                  end if
                  if blanc$=" " then
                                    commune$=mid$(ligne$,12,30)
                                    if dptecrit$="13" then
                                                          comm$=left$(commune$,9)
                                                          if comm$="MARSEILLE" then
                                                                                   m=m+1
                                                                                   if m>1 then
                                                                                              nbcom=nbcom-1
                                                                                              testd3=0
                                                                                   end if
                                                          end if
                                    end if
                                    if dptecrit$="69" then
                                                          comm$=left$(commune$,4)
                                                          if comm$="LYON" then
                                                                              l=l+1
                                                                              if l>1 then
                                                                                         nbcom=nbcom-1
                                                                                         testd3=0
                                                                              end if
                                                          end if
                                    end if
                                    if dptecrit$<>"75" then
                                                           nbcom=nbcom+1
                                    end if
                                    if dptesp$="        " then
                                                              if (dptecrit$="13" or dptecrit$="59" or dptecrit$="75" or dptecrit$="92") then
                                                                                                                                            np=np+1
                                                                                                                                            'locate 1,1
                                                                                                                                            'print "passer par la"
                                                                                                                                            if np>=1 then
                                                                                                                                                         n=n+1
                                                                                                                                                         'locate 2,1
                                                                                                                                                         'print "n=";n
                                                                                                                                                         nbl=nbl-1
                                                                                                                                                         nblt=nblt-1
                                                                                                                                                         goto suitecommune
                                                                                                                                            end if
                                                              end if
                                    end if
                                    if dptecrit$="75" then
                                                          comm$=left$(commune$,5)
                                                          if comm$="PARIS" then
                                                                               p=p+1
                                                                               nbcom=1
                                                                               if p>1 then
                                                                                          testd3=0
                                                                               end if
                                                          end if
                                                          
                                    end if
                                    print #2,space$(11);"commune :     ";commune$;" departement : ";departement$
                                    if comm$="MARSEILLE" then
                                                             comm$=""
                                                             commune$="MARSEILLE                     "
                                    end if
                                    if comm$="LYON" then
                                                        comm$=""
                                                        commune$="LYON                          "
                                    end if
                                    if comm$="PARIS" then
                                                         comm$=""
                                                         commune$="PARIS                         "
                                    end if
                                    if testd3=0 then
                                                    testd3=1
                                                    goto suitecommune
                                    end if
                                    print #3,commune$+","+dptecrit$
                                    nbcomtotal=nbcomtotal+1
                                    suitecommune :
                                else
                                    nom$=mid$(ligne$,12,4)
                                    if nom$="RLE " then
                                                       ec$="RUELLE     "
                                                   elseif nom$="RTE " then
                                                                          ec$="ROUTE      "
                                                   elseif nom$="PL  " then
                                                                          ec$="PLACE      "
                                                   elseif nom$="CHE " then
                                                                          ec$="CHEMIN     "
                                                   elseif nom$="PLA " then
                                                                          ec$="PLACE      "
                                                   elseif nom$="IMP " then
                                                                          ec$="IMPASSE    "
                                                   elseif nom$="PKG " then
                                                                          ec$="PARKING    "
                                                   elseif nom$="AV  " then
                                                                          ec$="AVENUE     "
                                                   elseif nom$="PAS " then
                                                                          ec$="PASSAGE    "
                                                   elseif nom$="SEN " then
                                                                          ec$="SENTIER    "
                                                   elseif nom$="RES " then
                                                                          ec$="RESIDENCE  "
                                                   elseif nom$="SQ  " then
                                                                          ec$="SQUARE     "
                                                   elseif nom$="ALL " then
                                                                          ec$="ALLEE      "
                                                   elseif nom$="RPT " then
                                                                          ec$="REMPART    "
                                                   elseif nom$="ESPA" then
                                                                          ec$="ESPACE     "
                                                   elseif nom$="CRX " then
                                                                          ec$="CROIX      "
                                                   elseif nom$="CHEM" then
                                                                          ec$="CHEMIN     "
                                                   elseif nom$="BD  " then
                                                                          ec$="BOULEVARD  "
                                                   elseif nom$="PIST" then
                                                                          ec$="PISTE      "
                                                   elseif nom$="DEV " then
                                                                          ec$="DEVIATION  "
                                                   elseif nom$="PLE " then
                                                                          ec$="PASSERELLE "
                                                   elseif nom$="JARD" then
                                                                          ec$="JARDIN     "
                                                   elseif nom$="PROM" then
                                                                          ec$="PROMENADE  "
                                                   elseif nom$="TSSE" then
                                                                          ec$="TERRASSE   "
                                                   elseif nom$="MTE " then
                                                                          ec$="MTE        "
                                                   elseif nom$="CAR " then
                                                                          ec$="CAR        "
                                                   elseif nom$="GR  " then
                                                                          ec$="GR         "
                                                   elseif nom$="PTA " then
                                                                          ec$="PTA        "
                                                   elseif nom$="ECL " then
                                                                          ec$="ECL        "
                                                   elseif nom$="CRS " then
                                                                          ec$="CRS        "
                                                   elseif nom$="COR " then
                                                                          ec$="COR        "
                                                   elseif nom$="CHS " then
                                                                          ec$="CHS        "
                                                   elseif nom$="VEN " then
                                                                          ec$="VEN        "
                                                   elseif nom$="PTTE" then
                                                                          ec$="PTTE       "
                                                   elseif nom$="DOM " then
                                                                          ec$="DOMAINE    "
                                                   elseif nom$="VALL" then
                                                                          ec$="VALLEE     "
                                                   elseif nom$="TRA " then
                                                                          ec$="TRA        "
                                                   elseif nom$="RAC " then
                                                                          ec$="RAC        "
                                                   elseif nom$="CTRE" then
                                                                          ec$="CENTRE     "
                                                   elseif nom$="GAL " then
                                                                          ec$="GALLERIE   "
                                                   elseif nom$="MAR " then
                                                                          ec$="MAR        "
                                                   elseif nom$="CAN " then
                                                                          ec$="CAN        "
                                                   elseif nom$="CAMI" then
                                                                          ec$="CAMI       "
                                                   elseif nom$="PAE " then
                                                                          ec$="PAE        "
                                                   elseif nom$="CALL" then
                                                                          ec$="CALL       "
                                                   elseif nom$="VLA " then
                                                                          ec$="VILLA      "
                                                   elseif nom$="ESP " then
                                                                          ec$="ESPLANADE  "
                                                   elseif nom$="PTE " then
                                                                          ec$="PORTE      "
                                                   elseif nom$="RUIS" then
                                                                          ec$="RUISSEAU   "
                                                   elseif nom$="CTR " then
                                                                          ec$="CENTRE     "
                                                   elseif nom$="VIAD" then
                                                                          ec$="VIADUC     "
                                                   elseif nom$="MAR " then
                                                                          ec$="MARCHE     "
                                                   elseif nom$="DSC " then
                                                                          ec$="DSC        "
                                                   elseif nom$="AUT " then
                                                                          ec$="AUTOROUTE  "
                                                   elseif blanc$="B" then
                                                                         ec$="LIEU-DIT   "
                                                                     else
                                                                         ec$=nom$+space$(7)
                                    end if
                                    ligneecrit$=ec$+mid$(ligne$,16,27)
                                    print #2,ligneecrit$
                  end if
              else
                  np=0
                  print #2,"           nombre de communes =";nbcom
                  'print #2,"           nombre de voies et lieux-dits =";nbl-1-nbcom
                  close #2
                  '
                  nbl=0
                  nbcom=0
                  test$=dpt$
                  'if dptecrit$="14" then                        ' arret apres la commune 14 par exemple
                  '                      goto attente
                  'end if
                  ficecrit$=disqueecrit$+debutnom$+dptecrit$+extension$
                  open ficecrit$ for output AS #2
                  print #2,debut$
                  nbl=nbl+1
                  nblt=nblt+1
                  nblt=nblt-2 'on enleve 2 pour les deux premieres lignes
                  departement$=mid$(ligne$,12,30)
                  print #2,dptecrit$+"         "+departement$
end if
findeboucle :
wend
'
locate 3,1
print space$(36)
'
'
'detruit$= "del c:lieu-999.txt"
'shell detruit$
'
attente :
close
locate 6,1
print "nb total de communes :";nbcomtotal
'locate 8,1
'print "nb total de voies et lieux-dits trait‚s :";nblt
'locate 10,1
'print "n=";n
'locate 11,1
'print "l=";l
'locate 12,1
'print "m=";m
locate 14,1
print "Appuyer sur une touche pour continuer"
'
fin :
r$=inkey$
if inkey$="" then goto fin else end

