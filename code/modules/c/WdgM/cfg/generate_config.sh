if [ "$1" == "" ]; then
 echo "usage: $0 arxml-file"
 exit 1
else
  ../generator/picea_wdgm_scg.exe -s AUTOSAR_4-0-3.xsd -x xml.xsd -a $1 -a AUTOSAR_Types.arxml -a WDGM_VID41_MD.arxml -a WdgM_VID41_Impl.arxml -a WdgM_VID41_SWCD.arxml -o ../generated_cfg
fi
