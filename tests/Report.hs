{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text
import qualified Pipes.ByteString as PB
import System.Directory
import System.IO
import Test.Hspec
import Test.HUnit.Base

import Data.WeatherBalloon
import Data.WeatherBalloon.Types

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "sortAndCount" $
        it "counts valid records correctly" doTestCount
    describe "accumAndOutput" $
        it "computes correct report data" doTestAccum

doTestCount :: IO ()
doTestCount = do
    fh <- openFile "tests/data/1000.txt" ReadMode
    (fn, report) <- runStateT (sortAndCount (PB.fromHandle fh)) initialReportState
    hClose fh
    (report ^. reportCount) @?= 787
    removeFile fn

doTestAccum :: IO ()
doTestAccum = do
    copyFile "tests/data/1000sorted.txt" ".report-test-in.tmp"
    report <- execStateT (accumAndOutput ".report-test-in.tmp" testOpts) testReportState
    removeFile ".report-test-out.tmp"
    (report ^. reportMean) @?= (Just $ Temperature 4.290978398983452)
    (report ^. reportMin) @?= (Just $ Temperature 1.0)
    (report ^. reportMax) @?= (Just $ Temperature 275.0)
    (report ^. reportDistance) @?= 8223193.482251396
    (report ^. reportObservatories) @?= expectedReportObservatories

testOpts :: WeatherOpts
testOpts = WeatherOpts {
    optReportMin = False
  , optReportMax = False
  , optReportMean = False
  , optReportDistance = False
  , optReportObservatories = False
  , optNormalizeTemperature = Fahrenheit
  , optNormalizeDistance = Miles
  , optOutputPath = ".report-test-out.tmp"
}

testReportState :: ReportState
testReportState = reportCount .~ 787 $ initialReportState

expectedReportObservatories :: Map Observatory Integer
expectedReportObservatories = M.fromList [(Observatory {unObservatory = "A"},9),(Observatory {unObservatory = "AB"},1),(Observatory {unObservatory = "ADYS"},1),(Observatory {unObservatory = "AF"},1),(Observatory {unObservatory = "AFRW"},1),(Observatory {unObservatory = "AGLHUU"},1),(Observatory {unObservatory = "AGTC"},1),(Observatory {unObservatory = "AGVX"},1),(Observatory {unObservatory = "AI"},1),(Observatory {unObservatory = "AJHES"},1),(Observatory {unObservatory = "AKQKPY"},1),(Observatory {unObservatory = "AL"},1),(Observatory {unObservatory = "ASMY"},1),(Observatory {unObservatory = "AU"},1),(Observatory {unObservatory = "AX"},1),(Observatory {unObservatory = "AZUS"},1),(Observatory {unObservatory = "B"},11),(Observatory {unObservatory = "BC"},1),(Observatory {unObservatory = "BEE"},1),(Observatory {unObservatory = "BHCH"},1),(Observatory {unObservatory = "BJ"},1),(Observatory {unObservatory = "BJCV"},1),(Observatory {unObservatory = "BKK"},1),(Observatory {unObservatory = "BLC"},1),(Observatory {unObservatory = "BMXFI"},1),(Observatory {unObservatory = "BPDPX"},1),(Observatory {unObservatory = "BPUX"},1),(Observatory {unObservatory = "BQU"},1),(Observatory {unObservatory = "BUORZ"},1),(Observatory {unObservatory = "BYPX"},1),(Observatory {unObservatory = "BYVMG"},1),(Observatory {unObservatory = "BZF"},1),(Observatory {unObservatory = "C"},8),(Observatory {unObservatory = "CDFO"},1),(Observatory {unObservatory = "CE"},1),(Observatory {unObservatory = "CF"},1),(Observatory {unObservatory = "CIXCOR"},1),(Observatory {unObservatory = "CK"},1),(Observatory {unObservatory = "CKE"},1),(Observatory {unObservatory = "CMPCNJ"},1),(Observatory {unObservatory = "CMZ"},1),(Observatory {unObservatory = "CN"},1),(Observatory {unObservatory = "CNMFJ"},1),(Observatory {unObservatory = "CP"},1),(Observatory {unObservatory = "CROEEH"},1),(Observatory {unObservatory = "CS"},1),(Observatory {unObservatory = "CU"},1),(Observatory {unObservatory = "CYL"},1),(Observatory {unObservatory = "D"},4),(Observatory {unObservatory = "DABAFSS"},1),(Observatory {unObservatory = "DBZRHGJ"},1),(Observatory {unObservatory = "DFLBAJ"},1),(Observatory {unObservatory = "DHA"},1),(Observatory {unObservatory = "DIX"},1),(Observatory {unObservatory = "DJ"},2),(Observatory {unObservatory = "DN"},1),(Observatory {unObservatory = "DOSV"},1),(Observatory {unObservatory = "DPUUUFK"},1),(Observatory {unObservatory = "DSIZ"},1),(Observatory {unObservatory = "DXAK"},1),(Observatory {unObservatory = "DXE"},1),(Observatory {unObservatory = "DXO"},1),(Observatory {unObservatory = "DY"},1),(Observatory {unObservatory = "DZ"},1),(Observatory {unObservatory = "DZJ"},1),(Observatory {unObservatory = "E"},8),(Observatory {unObservatory = "EAIPTI"},1),(Observatory {unObservatory = "EB"},1),(Observatory {unObservatory = "EBMN"},1),(Observatory {unObservatory = "ECIBES"},1),(Observatory {unObservatory = "ECX"},1),(Observatory {unObservatory = "ECZ"},1),(Observatory {unObservatory = "EDJ"},1),(Observatory {unObservatory = "EGKDZ"},1),(Observatory {unObservatory = "EGQQ"},1),(Observatory {unObservatory = "EH"},1),(Observatory {unObservatory = "EHSQ"},1),(Observatory {unObservatory = "EHZWVO"},1),(Observatory {unObservatory = "EJQ"},1),(Observatory {unObservatory = "EK"},1),(Observatory {unObservatory = "EKP"},1),(Observatory {unObservatory = "EM"},1),(Observatory {unObservatory = "EO"},1),(Observatory {unObservatory = "EQZ"},1),(Observatory {unObservatory = "EUKSKHO"},1),(Observatory {unObservatory = "EVDO"},1),(Observatory {unObservatory = "EWPA"},1),(Observatory {unObservatory = "EWTJX"},1),(Observatory {unObservatory = "EZ"},1),(Observatory {unObservatory = "EZIPH"},1),(Observatory {unObservatory = "EZOPBM"},1),(Observatory {unObservatory = "F"},5),(Observatory {unObservatory = "FA"},1),(Observatory {unObservatory = "FCEAH"},1),(Observatory {unObservatory = "FCG"},1),(Observatory {unObservatory = "FCLCL"},1),(Observatory {unObservatory = "FCSC"},1),(Observatory {unObservatory = "FCUX"},1),(Observatory {unObservatory = "FDXR"},1),(Observatory {unObservatory = "FET"},1),(Observatory {unObservatory = "FFUJ"},1),(Observatory {unObservatory = "FH"},1),(Observatory {unObservatory = "FHFTWGP"},1),(Observatory {unObservatory = "FI"},1),(Observatory {unObservatory = "FIRM"},1),(Observatory {unObservatory = "FMVJ"},1),(Observatory {unObservatory = "FO"},1),(Observatory {unObservatory = "FON"},1),(Observatory {unObservatory = "FQEHH"},1),(Observatory {unObservatory = "FR"},1),(Observatory {unObservatory = "FRYW"},1),(Observatory {unObservatory = "FSE"},1),(Observatory {unObservatory = "FTUC"},1),(Observatory {unObservatory = "FU"},1),(Observatory {unObservatory = "FV"},2),(Observatory {unObservatory = "FWJ"},1),(Observatory {unObservatory = "FYV"},1),(Observatory {unObservatory = "FZJ"},1),(Observatory {unObservatory = "G"},5),(Observatory {unObservatory = "GB"},1),(Observatory {unObservatory = "GEPXBU"},1),(Observatory {unObservatory = "GG"},1),(Observatory {unObservatory = "GHQ"},1),(Observatory {unObservatory = "GI"},1),(Observatory {unObservatory = "GIR"},1),(Observatory {unObservatory = "GJFDFN"},1),(Observatory {unObservatory = "GKAOFL"},1),(Observatory {unObservatory = "GMM"},1),(Observatory {unObservatory = "GNK"},1),(Observatory {unObservatory = "GR"},1),(Observatory {unObservatory = "GRX"},1),(Observatory {unObservatory = "GSGU"},1),(Observatory {unObservatory = "GTONA"},1),(Observatory {unObservatory = "GV"},1),(Observatory {unObservatory = "GWJU"},1),(Observatory {unObservatory = "GWVDV"},1),(Observatory {unObservatory = "GWWDHUYOLV"},1),(Observatory {unObservatory = "GWX"},1),(Observatory {unObservatory = "GY"},1),(Observatory {unObservatory = "GZA"},1),(Observatory {unObservatory = "H"},6),(Observatory {unObservatory = "HBW"},1),(Observatory {unObservatory = "HCGU"},1),(Observatory {unObservatory = "HCNCQ"},1),(Observatory {unObservatory = "HCPF"},1),(Observatory {unObservatory = "HE"},1),(Observatory {unObservatory = "HED"},1),(Observatory {unObservatory = "HHY"},1),(Observatory {unObservatory = "HI"},1),(Observatory {unObservatory = "HIVM"},1),(Observatory {unObservatory = "HJM"},1),(Observatory {unObservatory = "HLKV"},1),(Observatory {unObservatory = "HNXX"},1),(Observatory {unObservatory = "HOPCK"},1),(Observatory {unObservatory = "HPDN"},1),(Observatory {unObservatory = "HQ"},1),(Observatory {unObservatory = "HQN"},1),(Observatory {unObservatory = "HRM"},1),(Observatory {unObservatory = "HRTD"},1),(Observatory {unObservatory = "HRXUMQ"},1),(Observatory {unObservatory = "HT"},1),(Observatory {unObservatory = "HV"},1),(Observatory {unObservatory = "HXZSBK"},1),(Observatory {unObservatory = "HYDBO"},1),(Observatory {unObservatory = "I"},13),(Observatory {unObservatory = "IB"},1),(Observatory {unObservatory = "IC"},1),(Observatory {unObservatory = "ICHS"},1),(Observatory {unObservatory = "IDC"},1),(Observatory {unObservatory = "IEN"},1),(Observatory {unObservatory = "IFR"},1),(Observatory {unObservatory = "IFW"},1),(Observatory {unObservatory = "IGIIRSK"},1),(Observatory {unObservatory = "IHKAVV"},1),(Observatory {unObservatory = "IJCM"},1),(Observatory {unObservatory = "IK"},1),(Observatory {unObservatory = "IKH"},1),(Observatory {unObservatory = "IMRQ"},1),(Observatory {unObservatory = "INF"},1),(Observatory {unObservatory = "IOU"},1),(Observatory {unObservatory = "IQB"},1),(Observatory {unObservatory = "ITU"},1),(Observatory {unObservatory = "IUZGZW"},1),(Observatory {unObservatory = "IXVIQW"},1),(Observatory {unObservatory = "IZ"},1),(Observatory {unObservatory = "IZTKW"},1),(Observatory {unObservatory = "J"},4),(Observatory {unObservatory = "JA"},2),(Observatory {unObservatory = "JAB"},1),(Observatory {unObservatory = "JB"},1),(Observatory {unObservatory = "JBPJ"},1),(Observatory {unObservatory = "JC"},2),(Observatory {unObservatory = "JCXAI"},1),(Observatory {unObservatory = "JDLT"},1),(Observatory {unObservatory = "JE"},1),(Observatory {unObservatory = "JG"},1),(Observatory {unObservatory = "JGKL"},1),(Observatory {unObservatory = "JHCT"},1),(Observatory {unObservatory = "JHO"},1),(Observatory {unObservatory = "JI"},1),(Observatory {unObservatory = "JINU"},1),(Observatory {unObservatory = "JJ"},1),(Observatory {unObservatory = "JK"},1),(Observatory {unObservatory = "JLQZG"},1),(Observatory {unObservatory = "JM"},1),(Observatory {unObservatory = "JN"},2),(Observatory {unObservatory = "JRQ"},1),(Observatory {unObservatory = "JSEMZ"},1),(Observatory {unObservatory = "JT"},1),(Observatory {unObservatory = "JTEQZTA"},1),(Observatory {unObservatory = "JUG"},1),(Observatory {unObservatory = "JUHF"},1),(Observatory {unObservatory = "JW"},1),(Observatory {unObservatory = "JXVC"},1),(Observatory {unObservatory = "JY"},1),(Observatory {unObservatory = "K"},7),(Observatory {unObservatory = "KB"},1),(Observatory {unObservatory = "KD"},1),(Observatory {unObservatory = "KDOQ"},1),(Observatory {unObservatory = "KDW"},1),(Observatory {unObservatory = "KFKJCC"},1),(Observatory {unObservatory = "KGRC"},1),(Observatory {unObservatory = "KI"},1),(Observatory {unObservatory = "KJBDX"},1),(Observatory {unObservatory = "KJCI"},1),(Observatory {unObservatory = "KK"},1),(Observatory {unObservatory = "KLMW"},1),(Observatory {unObservatory = "KSKWZ"},1),(Observatory {unObservatory = "KT"},1),(Observatory {unObservatory = "KTO"},1),(Observatory {unObservatory = "KV"},1),(Observatory {unObservatory = "KVJLB"},1),(Observatory {unObservatory = "KWP"},1),(Observatory {unObservatory = "L"},12),(Observatory {unObservatory = "LA"},1),(Observatory {unObservatory = "LAJM"},1),(Observatory {unObservatory = "LDIQ"},1),(Observatory {unObservatory = "LENXR"},1),(Observatory {unObservatory = "LI"},1),(Observatory {unObservatory = "LJGB"},1),(Observatory {unObservatory = "LKTQS"},1),(Observatory {unObservatory = "LLU"},1),(Observatory {unObservatory = "LMB"},1),(Observatory {unObservatory = "LMDUIBK"},1),(Observatory {unObservatory = "LMRV"},1),(Observatory {unObservatory = "LO"},1),(Observatory {unObservatory = "LRA"},1),(Observatory {unObservatory = "LRQH"},1),(Observatory {unObservatory = "LSB"},1),(Observatory {unObservatory = "LTEV"},1),(Observatory {unObservatory = "LTGBT"},1),(Observatory {unObservatory = "LTKET"},1),(Observatory {unObservatory = "LU"},1),(Observatory {unObservatory = "LVXN"},1),(Observatory {unObservatory = "LVZSKSZ"},1),(Observatory {unObservatory = "LW"},1),(Observatory {unObservatory = "LWVS"},1),(Observatory {unObservatory = "LYD"},1),(Observatory {unObservatory = "M"},9),(Observatory {unObservatory = "MA"},1),(Observatory {unObservatory = "MBOAY"},1),(Observatory {unObservatory = "MEC"},1),(Observatory {unObservatory = "MGS"},1),(Observatory {unObservatory = "MH"},1),(Observatory {unObservatory = "MKE"},1),(Observatory {unObservatory = "MKH"},1),(Observatory {unObservatory = "MM"},2),(Observatory {unObservatory = "MMM"},1),(Observatory {unObservatory = "MO"},1),(Observatory {unObservatory = "MONL"},1),(Observatory {unObservatory = "MTWYG"},1),(Observatory {unObservatory = "MU"},1),(Observatory {unObservatory = "MV"},1),(Observatory {unObservatory = "MW"},1),(Observatory {unObservatory = "MXTFIU"},1),(Observatory {unObservatory = "N"},5),(Observatory {unObservatory = "NAEZL"},1),(Observatory {unObservatory = "NALJMC"},1),(Observatory {unObservatory = "NDF"},1),(Observatory {unObservatory = "NF"},1),(Observatory {unObservatory = "NG"},1),(Observatory {unObservatory = "NH"},1),(Observatory {unObservatory = "NHYIA"},1),(Observatory {unObservatory = "NJ"},1),(Observatory {unObservatory = "NJKXXJRSL"},1),(Observatory {unObservatory = "NO"},1),(Observatory {unObservatory = "NP"},1),(Observatory {unObservatory = "NQD"},1),(Observatory {unObservatory = "NSB"},1),(Observatory {unObservatory = "NSRXY"},1),(Observatory {unObservatory = "NTX"},1),(Observatory {unObservatory = "NU"},2),(Observatory {unObservatory = "NUEF"},1),(Observatory {unObservatory = "NUO"},1),(Observatory {unObservatory = "NUZP"},1),(Observatory {unObservatory = "NV"},1),(Observatory {unObservatory = "NVML"},1),(Observatory {unObservatory = "NWRJFF"},1),(Observatory {unObservatory = "NXXPYCN"},1),(Observatory {unObservatory = "NY"},2),(Observatory {unObservatory = "NZTAJE"},1),(Observatory {unObservatory = "NZU"},1),(Observatory {unObservatory = "O"},8),(Observatory {unObservatory = "OASA"},1),(Observatory {unObservatory = "OB"},1),(Observatory {unObservatory = "OBPXQA"},1),(Observatory {unObservatory = "OCFJV"},1),(Observatory {unObservatory = "OCP"},1),(Observatory {unObservatory = "OCRZ"},1),(Observatory {unObservatory = "OD"},1),(Observatory {unObservatory = "OGC"},1),(Observatory {unObservatory = "OI"},1),(Observatory {unObservatory = "OJB"},1),(Observatory {unObservatory = "OKLZPDE"},1),(Observatory {unObservatory = "OM"},1),(Observatory {unObservatory = "OODA"},1),(Observatory {unObservatory = "OOGB"},1),(Observatory {unObservatory = "OQZU"},1),(Observatory {unObservatory = "ORSGS"},1),(Observatory {unObservatory = "OSS"},1),(Observatory {unObservatory = "OX"},1),(Observatory {unObservatory = "OXO"},1),(Observatory {unObservatory = "OYHU"},1),(Observatory {unObservatory = "OYW"},1),(Observatory {unObservatory = "P"},8),(Observatory {unObservatory = "PA"},1),(Observatory {unObservatory = "PBCW"},1),(Observatory {unObservatory = "PCJ"},1),(Observatory {unObservatory = "PCL"},1),(Observatory {unObservatory = "PD"},1),(Observatory {unObservatory = "PDQ"},1),(Observatory {unObservatory = "PDVR"},1),(Observatory {unObservatory = "PEA"},1),(Observatory {unObservatory = "PEK"},1),(Observatory {unObservatory = "PEOIV"},1),(Observatory {unObservatory = "PER"},1),(Observatory {unObservatory = "PGO"},1),(Observatory {unObservatory = "PGP"},1),(Observatory {unObservatory = "PHMS"},1),(Observatory {unObservatory = "PIK"},1),(Observatory {unObservatory = "PISCZRA"},1),(Observatory {unObservatory = "PLSOTA"},1),(Observatory {unObservatory = "PM"},1),(Observatory {unObservatory = "POL"},1),(Observatory {unObservatory = "PRGC"},1),(Observatory {unObservatory = "PRQ"},1),(Observatory {unObservatory = "PSRPF"},1),(Observatory {unObservatory = "PUUZ"},1),(Observatory {unObservatory = "PZW"},1),(Observatory {unObservatory = "Q"},7),(Observatory {unObservatory = "QA"},1),(Observatory {unObservatory = "QB"},1),(Observatory {unObservatory = "QEE"},1),(Observatory {unObservatory = "QEXW"},1),(Observatory {unObservatory = "QH"},1),(Observatory {unObservatory = "QI"},1),(Observatory {unObservatory = "QIAQ"},1),(Observatory {unObservatory = "QICUB"},1),(Observatory {unObservatory = "QJEDSXP"},1),(Observatory {unObservatory = "QJFV"},1),(Observatory {unObservatory = "QKSSG"},1),(Observatory {unObservatory = "QKX"},1),(Observatory {unObservatory = "QL"},2),(Observatory {unObservatory = "QLHT"},1),(Observatory {unObservatory = "QM"},1),(Observatory {unObservatory = "QNRP"},1),(Observatory {unObservatory = "QOURS"},1),(Observatory {unObservatory = "QPRYDJ"},1),(Observatory {unObservatory = "QQCR"},1),(Observatory {unObservatory = "QQMM"},1),(Observatory {unObservatory = "QSO"},1),(Observatory {unObservatory = "QT"},1),(Observatory {unObservatory = "QTXZ"},1),(Observatory {unObservatory = "QTZ"},1),(Observatory {unObservatory = "QUH"},1),(Observatory {unObservatory = "QXYQJ"},1),(Observatory {unObservatory = "QYNXN"},1),(Observatory {unObservatory = "QZRB"},1),(Observatory {unObservatory = "R"},4),(Observatory {unObservatory = "RAOHB"},1),(Observatory {unObservatory = "RE"},1),(Observatory {unObservatory = "RFG"},1),(Observatory {unObservatory = "RI"},1),(Observatory {unObservatory = "RK"},1),(Observatory {unObservatory = "RP"},1),(Observatory {unObservatory = "RPC"},1),(Observatory {unObservatory = "RQT"},1),(Observatory {unObservatory = "RRBF"},1),(Observatory {unObservatory = "RT"},1),(Observatory {unObservatory = "RTH"},1),(Observatory {unObservatory = "RTWVN"},1),(Observatory {unObservatory = "RUQ"},1),(Observatory {unObservatory = "RUX"},1),(Observatory {unObservatory = "RVQP"},1),(Observatory {unObservatory = "RVTT"},1),(Observatory {unObservatory = "RW"},1),(Observatory {unObservatory = "RZ"},1),(Observatory {unObservatory = "S"},7),(Observatory {unObservatory = "SA"},1),(Observatory {unObservatory = "SB"},1),(Observatory {unObservatory = "SCXQ"},1),(Observatory {unObservatory = "SDPDJ"},1),(Observatory {unObservatory = "SDWPB"},1),(Observatory {unObservatory = "SEGPM"},1),(Observatory {unObservatory = "SERMCF"},1),(Observatory {unObservatory = "SKC"},1),(Observatory {unObservatory = "SKPT"},1),(Observatory {unObservatory = "SL"},1),(Observatory {unObservatory = "SM"},1),(Observatory {unObservatory = "SNV"},1),(Observatory {unObservatory = "STB"},1),(Observatory {unObservatory = "STFBB"},1),(Observatory {unObservatory = "SU"},2),(Observatory {unObservatory = "SUBTBX"},1),(Observatory {unObservatory = "SUSXI"},1),(Observatory {unObservatory = "SVYAX"},1),(Observatory {unObservatory = "SX"},1),(Observatory {unObservatory = "SY"},1),(Observatory {unObservatory = "SYKE"},1),(Observatory {unObservatory = "T"},11),(Observatory {unObservatory = "TCOR"},1),(Observatory {unObservatory = "TCS"},1),(Observatory {unObservatory = "TD"},1),(Observatory {unObservatory = "TDQ"},1),(Observatory {unObservatory = "TEQYXDI"},1),(Observatory {unObservatory = "TF"},2),(Observatory {unObservatory = "TFJG"},1),(Observatory {unObservatory = "TFJPA"},1),(Observatory {unObservatory = "TFWVI"},1),(Observatory {unObservatory = "TGG"},1),(Observatory {unObservatory = "THW"},1),(Observatory {unObservatory = "TLVMFU"},1),(Observatory {unObservatory = "TLYG"},1),(Observatory {unObservatory = "TMIO"},1),(Observatory {unObservatory = "TMX"},1),(Observatory {unObservatory = "TNJGL"},1),(Observatory {unObservatory = "TOS"},1),(Observatory {unObservatory = "TRH"},1),(Observatory {unObservatory = "TS"},1),(Observatory {unObservatory = "TSB"},1),(Observatory {unObservatory = "TT"},1),(Observatory {unObservatory = "TTLK"},1),(Observatory {unObservatory = "TVRW"},1),(Observatory {unObservatory = "TZ"},2),(Observatory {unObservatory = "TZCHBS"},1),(Observatory {unObservatory = "U"},4),(Observatory {unObservatory = "UBWG"},1),(Observatory {unObservatory = "UDN"},1),(Observatory {unObservatory = "UFQQY"},1),(Observatory {unObservatory = "UGLQ"},1),(Observatory {unObservatory = "UGQ"},1),(Observatory {unObservatory = "UHES"},1),(Observatory {unObservatory = "UIGEG"},1),(Observatory {unObservatory = "UIU"},1),(Observatory {unObservatory = "UJM"},1),(Observatory {unObservatory = "UM"},1),(Observatory {unObservatory = "UMKGNYV"},1),(Observatory {unObservatory = "UOPW"},1),(Observatory {unObservatory = "UOVME"},1),(Observatory {unObservatory = "UP"},1),(Observatory {unObservatory = "UQ"},1),(Observatory {unObservatory = "UQC"},1),(Observatory {unObservatory = "URL"},1),(Observatory {unObservatory = "US"},1),(Observatory {unObservatory = "UT"},1),(Observatory {unObservatory = "UTAOV"},1),(Observatory {unObservatory = "UV"},1),(Observatory {unObservatory = "UWM"},1),(Observatory {unObservatory = "UYZ"},1),(Observatory {unObservatory = "V"},16),(Observatory {unObservatory = "VB"},1),(Observatory {unObservatory = "VC"},1),(Observatory {unObservatory = "VE"},1),(Observatory {unObservatory = "VJCNW"},1),(Observatory {unObservatory = "VKKB"},1),(Observatory {unObservatory = "VL"},1),(Observatory {unObservatory = "VLZBV"},1),(Observatory {unObservatory = "VNG"},1),(Observatory {unObservatory = "VNTRR"},1),(Observatory {unObservatory = "VO"},1),(Observatory {unObservatory = "VPWEN"},1),(Observatory {unObservatory = "VQ"},1),(Observatory {unObservatory = "VQOXO"},1),(Observatory {unObservatory = "VQZ"},1),(Observatory {unObservatory = "VS"},2),(Observatory {unObservatory = "VSK"},1),(Observatory {unObservatory = "VT"},1),(Observatory {unObservatory = "VUUKJJEPV"},1),(Observatory {unObservatory = "VVS"},1),(Observatory {unObservatory = "VW"},1),(Observatory {unObservatory = "VWIP"},1),(Observatory {unObservatory = "VWPJU"},1),(Observatory {unObservatory = "VXJSK"},1),(Observatory {unObservatory = "VYLPMP"},1),(Observatory {unObservatory = "VYMOR"},1),(Observatory {unObservatory = "VZ"},1),(Observatory {unObservatory = "VZBTHH"},1),(Observatory {unObservatory = "W"},8),(Observatory {unObservatory = "WAO"},1),(Observatory {unObservatory = "WAUM"},1),(Observatory {unObservatory = "WET"},1),(Observatory {unObservatory = "WFNSL"},1),(Observatory {unObservatory = "WI"},1),(Observatory {unObservatory = "WKLL"},1),(Observatory {unObservatory = "WLX"},1),(Observatory {unObservatory = "WN"},1),(Observatory {unObservatory = "WNXCW"},1),(Observatory {unObservatory = "WQ"},1),(Observatory {unObservatory = "WT"},2),(Observatory {unObservatory = "WTGGCYE"},1),(Observatory {unObservatory = "WUP"},1),(Observatory {unObservatory = "WVO"},1),(Observatory {unObservatory = "WWZ"},1),(Observatory {unObservatory = "WY"},1),(Observatory {unObservatory = "WZG"},1),(Observatory {unObservatory = "WZXV"},1),(Observatory {unObservatory = "X"},11),(Observatory {unObservatory = "XA"},1),(Observatory {unObservatory = "XB"},1),(Observatory {unObservatory = "XBI"},1),(Observatory {unObservatory = "XC"},1),(Observatory {unObservatory = "XCSP"},1),(Observatory {unObservatory = "XCU"},1),(Observatory {unObservatory = "XE"},1),(Observatory {unObservatory = "XGG"},1),(Observatory {unObservatory = "XHFJYRRYW"},1),(Observatory {unObservatory = "XHQY"},1),(Observatory {unObservatory = "XK"},2),(Observatory {unObservatory = "XLMK"},1),(Observatory {unObservatory = "XMHBMO"},1),(Observatory {unObservatory = "XNH"},1),(Observatory {unObservatory = "XQ"},1),(Observatory {unObservatory = "XR"},1),(Observatory {unObservatory = "XRTES"},1),(Observatory {unObservatory = "XSN"},1),(Observatory {unObservatory = "XTXL"},1),(Observatory {unObservatory = "XWGTGD"},1),(Observatory {unObservatory = "XWP"},1),(Observatory {unObservatory = "XWY"},1),(Observatory {unObservatory = "XXAGAN"},1),(Observatory {unObservatory = "XXWYQX"},1),(Observatory {unObservatory = "XZFWO"},1),(Observatory {unObservatory = "XZYS"},1),(Observatory {unObservatory = "Y"},6),(Observatory {unObservatory = "YBRIBJ"},1),(Observatory {unObservatory = "YBW"},1),(Observatory {unObservatory = "YCY"},1),(Observatory {unObservatory = "YHFF"},1),(Observatory {unObservatory = "YL"},2),(Observatory {unObservatory = "YLIZS"},1),(Observatory {unObservatory = "YMK"},1),(Observatory {unObservatory = "YNQ"},1),(Observatory {unObservatory = "YNX"},1),(Observatory {unObservatory = "YO"},1),(Observatory {unObservatory = "YP"},1),(Observatory {unObservatory = "YQ"},1),(Observatory {unObservatory = "YRZSL"},1),(Observatory {unObservatory = "YT"},1),(Observatory {unObservatory = "YUKTGUT"},1),(Observatory {unObservatory = "YUN"},1),(Observatory {unObservatory = "YXWH"},1),(Observatory {unObservatory = "YYH"},1),(Observatory {unObservatory = "YYY"},1),(Observatory {unObservatory = "Z"},10),(Observatory {unObservatory = "ZA"},1),(Observatory {unObservatory = "ZC"},1),(Observatory {unObservatory = "ZDA"},1),(Observatory {unObservatory = "ZDX"},1),(Observatory {unObservatory = "ZE"},1),(Observatory {unObservatory = "ZEH"},1),(Observatory {unObservatory = "ZEWRPRF"},1),(Observatory {unObservatory = "ZEZ"},1),(Observatory {unObservatory = "ZF"},1),(Observatory {unObservatory = "ZG"},1),(Observatory {unObservatory = "ZGMYUUTXCX"},1),(Observatory {unObservatory = "ZGUOVIGRZ"},1),(Observatory {unObservatory = "ZGVHB"},1),(Observatory {unObservatory = "ZN"},2),(Observatory {unObservatory = "ZNOUUC"},1),(Observatory {unObservatory = "ZNVB"},1),(Observatory {unObservatory = "ZPIDL"},1),(Observatory {unObservatory = "ZR"},1),(Observatory {unObservatory = "ZRTC"},1),(Observatory {unObservatory = "ZSY"},1),(Observatory {unObservatory = "ZTV"},1),(Observatory {unObservatory = "ZVFET"},1),(Observatory {unObservatory = "ZW"},1),(Observatory {unObservatory = "ZX"},1),(Observatory {unObservatory = "ZXU"},1),(Observatory {unObservatory = "ZZBPT"},1)]