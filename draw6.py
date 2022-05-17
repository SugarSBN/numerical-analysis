'''
Author: SuBonan
Date: 2022-05-15 16:12:06
LastEditTime: 2022-05-15 16:20:09
FilePath: \数值计算\draw6.py
Github: https://github.com/SugarSBN
これなに、これなに、これない、これなに、これなに、これなに、ねこ！ヾ(*´∀｀*)ﾉ
'''
import matplotlib.pyplot as plt
import numpy as np

x = [0.0,0.1,0.2,0.30000000000000004,0.4,0.5,0.6000000000000001,0.7000000000000001,0.8,0.9,1.0,1.1,1.2000000000000002,1.3,1.4000000000000001,1.5,1.6,1.7000000000000002,1.8,1.9000000000000001,2.0,2.1,2.2,2.3000000000000003,2.4000000000000004,2.5,2.6,2.7,2.8000000000000003,2.9000000000000004,3.0,3.1,3.2,3.3000000000000003,3.4000000000000004,3.5,3.6,3.7,3.8000000000000003,3.9000000000000004,4.0,4.1000000000000005,4.2,4.3,4.4,4.5,4.6000000000000005,4.7,4.800000000000001,4.9,5.0,5.1000000000000005,5.2,5.300000000000001,5.4,5.5,5.6000000000000005,5.7,5.800000000000001,5.9,6.0,6.1000000000000005,6.2,6.300000000000001,6.4,6.5,6.6000000000000005,6.7,6.800000000000001,6.9,7.0,7.1000000000000005,7.2,7.300000000000001,7.4,7.5,7.6000000000000005,7.7,7.800000000000001,7.9,8.0,8.1,8.200000000000001,8.3,8.4,8.5,8.6,8.700000000000001,8.8,8.9,9.0,9.1,9.200000000000001,9.3,9.4,9.5,9.600000000000001,9.700000000000001,9.8,9.9,10.0]
y = [1.0,0.9949984761818907,0.9800155102921466,0.9551481570110951,0.9205950241227443,0.8766503908843866,0.8237024614158424,0.7622294966411843,0.6927955882382407,0.6160456535248231,0.5326998132372381,0.44354714024174957,0.3494388037658817,0.25128061596614826,0.15002499041682266,4.6662325606077526e-2,-5.7788163253468625e-2,-0.16228812571627504,-0.2657902129108889,-0.3672485889427498,-0.4656297119583481,-0.5599232562254333,-0.6491530254783321,-0.7323876913184533,-0.8087511804206806,-0.8774325319895718,-0.9376950530297894,-0.9888846135195524,-1.030436945736726,-1.0618838403289315,-1.0828581642318247,-1.0930976598551871,-1.0924475185831342,-1.0808617522021982,-1.0584034113552059,-1.0252437190224346,-0.9816601985030138,-0.9280338792530386,-0.8648456607543294,-0.7926719054629588,-0.7121793184243613,-0.6241191552779803,-0.529320784211375,-0.4286846130626705,-0.32317438214970307,-0.2138088181504621,-0.10165264565528893,1.219303851278239e-2,0.12660100745791117,0.24042871993748005,0.35252963940768167,0.46176495557745106,0.5670155762775799,0.6671942279579904,0.7612574782805066,0.8482174766235626,0.9271531999194437,0.9972209934361211,1.057664209319343,1.1078217693582884,1.1471355109929644,1.175156214674574,1.1915482533550517,1.1960928478578874,1.1886899519459708,1.1693588251866922,1.1382373779770614,1.09558038990164,1.0417567094297149,0.9772455401987004,0.9026319079863442,0.8186013848324004,0.7259341250107347,0.6254982443401689,0.5182425523956964,0.40518862915058956,0.28742222573628456,0.1660839651541291,4.235932407858463e-2,-8.253210823308085e-2,-0.20734807389037496,-0.3308357412759356,-0.4517443614188395,-0.5688383558912313,-0.6809106663440209,-0.7867961599985624,-0.8853848572958005,-0.9756347305461746,-1.0565838181099305,-1.127361408437558,-1.1871980723937532,-1.2354343580127185,-1.2715280109428935,-1.2950596261178278,-1.3057367293179991,-1.3033961959879086,-1.2880054968737136,-1.2596612957020694,-1.2185914160853777,-1.1651383382634566,-1.0998063911496796]
y1 = [1.0,0.9949984900623116,0.9800151464197512,0.9551430454394212,0.9205682087214799,0.8765595322603148,0.8234640749057633,0.7617018541009166,0.6917621119910511,0.6142007121493035,0.5296387599233865,0.4387622294245364,0.3423222841128732,0.24113584023980517,0.1360858212851619,2.8120475687261046e-2,-8.174890400561122e-2,-0.1924525039723187,-0.3028693137525342,-0.41183765533817807,-0.5181690876164987,-0.6206655609315072,-0.7181393600564556,-0.8094350345986425,-0.8934522209521908,-0.9691680584853434,-1.0356578355875719,-1.0921125910105285,-1.1378526396387405,-1.172336359528508,-1.1951640171302305,-1.2060768573068765,-1.2049520832154956,-1.1917946510525295,-1.1667269797059097,-1.129977721601674,-1.0818706733787236,-1.0228147502339995,-0.9532957366818092,-0.8738702873771095,-0.7851624061848212,-0.6878623933061427,-0.5827280251953075,-0.470587521654192,-0.3523436589491241,-0.2289782093446363,-0.10155573376543023,2.8774359450224912e-2,0.16077862752320843,0.2931443747871042,0.42448933761529745,0.5533769026191581,0.6783370363618126,0.7978925300545988,0.9105895216908482,1.0150306387432264,1.1099086132560048,1.1940379605153293,1.2663823546371324,1.3260756981476967,1.3724355228964773,1.4049681747824698,1.42336609022095,1.4274982331171455,1.4173953227884797,1.393231790937823,1.3553064579302236,1.304023756692402,1.239877022290912,1.1634349770070742,1.0753321340708932,0.9762634583742446,0.8669832785913094,0.7483081434868228,0.6211230453864502,0.4863901809830308,0.34515917187549483,0.19857742203677822,4.789905942576139e-2,-0.10550927449830755,-0.26016761040864145,-0.414488561326002,-0.5667914075942415,-0.7153252347536128,-0.8583011527262193,-0.9939325568416536,-1.1204812393839356,-1.2363061217922378,-1.3399106746563367,-1.429984913671373,-1.5054382985492512,-1.5654208752257401,-1.6093314178417475,-1.6368128598491951,-1.6477367418774505,-1.642179256657494,-1.620392620368182,-1.5827732266715044,-1.5298363315871164,-1.4621741787529465,-1.3804941233554395]
y2 = [1.0,0.9949986288868914,0.9800115061788047,0.9550918568102017,0.9202989204483599,0.8756424576828419,0.8210387951450197,0.7562730414611547,0.6809688523997752,0.5945658796542418,0.4963076416019832,0.385246295161211,0.26027713785852913,0.12022504975434076,-3.598265080568824e-2,-0.20901060792259132,-0.3986300577897627,-0.6030411340070602,-0.8180966433759551,-1.0367200290227505,-1.249025854249327,-1.4436299108180097,-1.6100861056288653,-1.7414775032422383,-1.8357845803747737,-1.8953972374079886,-1.925373937251169,-1.9315763126778092,-1.9194063999231226,-1.8932474199156442,-1.8563817678917323,-1.8111334572497773,-1.7590717131474096,-1.7011988560792841,-1.6380974786458036,-1.5700356453674071,-1.4970369136142112,-1.418923074238384,-1.335336169346854,-1.2457445439442307,-1.1494361875852481,-1.0455017180419048,-0.9328092855291212,-0.8099748538777832,-0.675334579701918,-0.5269328812944858,-0.36255271190702565,-0.17983669966378937,2.3418911645243567e-2,0.248677659128293,0.49565352656625844,0.760852321313807,1.0359941764472222,1.3072986209355044,1.5570871144376972,1.7683135673372559,1.9301088800910786,2.040686270064968,2.1058566985951743,2.1350354064283135,2.1376971825354163,2.1215818547845715,2.092322223691082,2.053746526571945,2.0083455570602595,1.957685555541595,1.902712748947161,1.843959455991213,1.7816769346797385,1.7159179741380168,1.646586078566113,1.573462445923901,1.4962177768245246,1.4144131222553593,1.327492138720322,1.2347659747866382,1.1353914053292304,1.0283427596793635,0.9123788646697584,0.7860081613639542,0.6474593712966963,0.49467336488719393,0.3253469734376424,0.13708467710034297,-7.224959665474795e-2,-0.3038513802611522,-0.5568415126631696,-0.8267066874695586,-1.1037839888309087,-1.3729556717653515,-1.6160047868031944,-1.816755476385042,-1.9663916702262818,-2.0653252644039237,-2.1207636200781703,-2.1425201663330724,-2.139824530305767,-2.1199392237719104,-2.088035873809008,-2.0475624830961068,-2.000797986841788]
plt.xlabel('x')
plt.ylabel('y')
l1, = plt.plot(x, y, 'g-', label = 'mu=0.01')
l2, = plt.plot(x, y1, 'b-', label = 'mu=0.1')
l3, = plt.plot(x, y2, 'k-', label = 'mu=1')
plt.legend(handles=[l1, l2, l3],labels=['mu=0.01', 'mu=0.1', 'mu=1'],loc='best')  
plt.show()  