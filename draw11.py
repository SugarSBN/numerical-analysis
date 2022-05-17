'''
Author: SuBonan
Date: 2022-05-17 16:56:39
LastEditTime: 2022-05-17 16:56:40
FilePath: \数值计算\draw11.py
Github: https://github.com/SugarSBN
これなに、これなに、これない、これなに、これなに、これなに、ねこ！ヾ(*´∀｀*)ﾉ
'''
import matplotlib.pyplot as plt
import numpy as np

x = [0.0,3.1415926535897934e-2,6.283185307179587e-2,9.42477796076938e-2,0.12566370614359174,0.15707963267948966,0.1884955592153876,0.21991148575128555,0.25132741228718347,0.2827433388230814,0.3141592653589793,0.3455751918948773,0.3769911184307752,0.4084070449666731,0.4398229715025711,0.471238898038469,0.5026548245743669,0.5340707511102649,0.5654866776461628,0.5969026041820608,0.6283185307179586,0.6597344572538566,0.6911503837897546,0.7225663103256524,0.7539822368615504,0.7853981633974484,0.8168140899333463,0.8482300164692442,0.8796459430051422,0.9110618695410401,0.942477796076938,0.9738937226128359,1.0053096491487339,1.0367255756846319,1.0681415022205298,1.0995574287564276,1.1309733552923256,1.1623892818282235,1.1938052083641215,1.2252211349000195,1.2566370614359172,1.2880529879718152,1.3194689145077132,1.3508848410436112,1.3823007675795091,1.4137166941154071,1.4451326206513049,1.4765485471872029,1.5079644737231008,1.5393804002589988,1.5707963267948968,1.6022122533307945,1.6336281798666925,1.6650441064025905,1.6964600329384885,1.7278759594743864,1.7592918860102844,1.7907078125461822,1.8221237390820801,1.8535396656179781,1.884955592153876,1.916371518689774,1.9477874452256718,1.9792033717615698,2.0106192982974678,2.0420352248333655,2.0734511513692637,2.1048670779051615,2.1362830044410597,2.1676989309769574,2.199114857512855,2.2305307840487534,2.261946710584651,2.2933626371205493,2.324778563656447,2.356194490192345,2.387610416728243,2.419026343264141,2.450442269800039,2.4818581963359367,2.5132741228718345,2.5446900494077327,2.5761059759436304,2.6075219024795286,2.6389378290154264,2.6703537555513246,2.7017696820872223,2.73318560862312,2.7646015351590183,2.796017461694916,2.8274333882308142,2.858849314766712,2.8902652413026098,2.921681167838508,2.9530970943744057,2.984513020910304,3.0159289474462017,3.0473448739820994,3.0787608005179976,3.1101767270538954,3.1415926535897936]
y = [0.0,5.2060258097173856e-3,1.9883722854220937e-2,4.262252055611396e-2,7.201184833799974e-2,0.10664113562248159,0.1450998118321628,0.1859773063896467,0.22786304871753665,0.2693464682384357,0.3090169943749474,0.34575615420939604,0.37961386546298975,0.4109321435166578,0.44005300375132955,0.467318461547934,0.49307053228740066,0.5176512313506586,0.5414025741186372,0.5646665759722655,0.5877852522924731,0.611023412959603,0.6343370458516553,0.6576049333460436,0.6807058578201823,0.7035186016514849,0.7259219472173657,0.7477946768952383,0.7690155730625168,0.7894634180966152,0.8090169943749476,0.8275774406926004,0.8451353215153528,0.8617035577266562,0.8772950702099626,0.8919227798487231,0.9055996075263901,0.918338474126415,0.9301523005322494,0.941054007627345,0.9510565162951535,0.9601682791352949,0.9683798756120617,0.9756774169059146,0.9820470141973143,0.9874747786667217,0.9919468214945975,0.9954492538614026,0.9979681869475976,0.999489731933643,1.0,0.9994897319336423,0.9979681869475959,0.9954492538614005,0.991946821494595,0.9874747786667188,0.9820470141973113,0.9756774169059117,0.9683798756120594,0.9601682791352935,0.9510565162951534,0.9410540076273466,0.930152300532253,0.9183384741264204,0.9055996075263973,0.8919227798487318,0.8772950702099712,0.8617035577266648,0.8451353215153596,0.8275774406926046,0.8090169943749475,0.789463418096609,0.7690155730625037,0.747794676895218,0.7259219472173392,0.7035186016514537,0.6807058578201488,0.6576049333460114,0.6343370458516283,0.6110234129595866,0.5877852522924731,0.564666575972288,0.5414025741186855,0.5176512313507331,0.49307053228749886,0.46731846154804946,0.4400530037514532,0.4109321435167774,0.3796138654630892,0.3457561542094568,0.30901699437494695,0.2693464682383517,0.2278630487173558,0.18597730638936721,0.14509981183179577,0.1066411356220491,7.201184833753685e-2,4.2622520555667204e-2,1.9883722853848457e-2,5.206025809490006e-3,0.0]
y1 = [0.0,3.141075907812829e-2,6.279051952931337e-2,9.410831331851433e-2,0.12533323356430426,0.15643446504023087,0.18738131458572463,0.21814324139654256,0.2486898871648548,0.2789911060392293,0.3090169943749474,0.3387379202452914,0.368124552684678,0.3971478906347806,0.4257792915650727,0.4539904997395468,0.4817536741017153,0.5090414157503713,0.5358267949789967,0.5620833778521306,0.5877852522924731,0.6129070536529765,0.6374239897486897,0.6613118653236518,0.6845471059286887,0.7071067811865476,0.7289686274214116,0.7501110696304596,0.7705132427757893,0.7901550123756904,0.8090169943749475,0.8270805742745618,0.8443279255020151,0.8607420270039436,0.8763066800438637,0.8910065241883678,0.9048270524660196,0.9177546256839811,0.9297764858882515,0.9408807689542256,0.9510565162951535,0.9602936856769431,0.9685831611286311,0.9759167619387474,0.9822872507286887,0.9876883405951378,0.9921147013144779,0.99556196460308,0.9980267284282716,0.9995065603657316,1.0,0.9995065603657316,0.9980267284282716,0.99556196460308,0.9921147013144778,0.9876883405951377,0.9822872507286886,0.9759167619387474,0.9685831611286311,0.9602936856769431,0.9510565162951535,0.9408807689542255,0.9297764858882513,0.9177546256839811,0.9048270524660195,0.8910065241883679,0.8763066800438635,0.8607420270039436,0.844327925502015,0.8270805742745617,0.8090169943749475,0.7901550123756903,0.7705132427757893,0.7501110696304594,0.7289686274214114,0.7071067811865476,0.6845471059286885,0.6613118653236518,0.6374239897486895,0.6129070536529764,0.5877852522924732,0.5620833778521305,0.5358267949789967,0.5090414157503711,0.4817536741017152,0.45399049973954647,0.4257792915650725,0.3971478906347806,0.36812455268467775,0.3387379202452913,0.3090169943749471,0.2789911060392291,0.24868988716485482,0.21814324139654231,0.18738131458572457,0.15643446504023054,0.1253332335643041,9.410831331851435e-2,6.279051952931314e-2,3.1410759078128236e-2,-3.216285744678249e-16]
y2 = [0.0,9.957388434552092e-3,3.613006177830945e-2,7.296748740881384e-2,0.11491913270360707,0.1564344650402309,0.1931295054267085,0.2252864893929888,0.2543542060995018,0.28178144470667793,0.3090169943749474,0.3371973520286747,0.36620984564796194,0.39562951097684546,0.42503138375936156,0.4539904997395468,0.4821660088506029,0.5095535177823941,0.5362327474139498,0.5622834186242995,0.5877852522924731,0.6127960068408586,0.637285590865278,0.6612019505049122,0.6844930318989417,0.7071067811865476,0.7289977311747134,0.7501467613416364,0.7705413378333164,0.7901689267957533,0.8090169943749476,0.8270720522973476,0.8443167946111974,0.860732960945189,0.8763022909280151,0.8910065241883678,0.9048285554720428,0.9177558999932479,0.9297772280832941,0.9408812100734923,0.9510565162951535,0.9602924728245797,0.9685810287180355,0.9759147887767768,0.982286357802059,0.9876883405951378,0.9921141769242975,0.9955606464259364,0.9980253637034817,0.9995059433603606,1.0,0.9995059433581117,0.998025363699546,0.9955606464214385,0.992114176920924,0.9876883405951378,0.9822863578076813,0.9759147887880216,0.9685810287320917,0.9602924728358246,0.9510565162951535,0.9408812100532515,0.9297772280422504,0.9177558999415216,0.9048285554304368,0.8910065241883679,0.8763022910033555,0.8607329610981191,0.8443167948040465,0.8270720524525267,0.8090169943749476,0.7901689265146316,0.7705413372626394,0.750146760621965,0.728997730595603,0.7071067811865477,0.6844930329480873,0.6612019526346892,0.6372855935511134,0.6127960090021212,0.5877852522924734,0.5622834147088382,0.5362327394655185,0.5095535077587231,0.4821660007846627,0.45399049973954647,0.42503139837206066,0.3956295406407936,0.3662098830568091,0.33719738213117245,0.3090169943749471,0.28178139017134207,0.2543540953921405,0.22528634978126955,0.1931293930826573,0.15643446504023042,0.11491933623225055,7.296790057431082e-2,3.613058281633753e-2,9.957807708258548e-3,0.0]
plt.xlabel('x')
plt.ylabel('y')
plt.subplot(2,2,1)
l1, = plt.plot(x, y, 'g-', label = 'spline 11.')
l2, = plt.plot(x, y1, 'b-', label = 'y=sin(x)')
l3, = plt.plot(x, y2, 'k-', label = 'spline 21.')
plt.legend(handles=[l1, l2, l3],labels=['spline 11', 'y=sin(x)', 'spline 21.'],loc='best')  
plt.subplot(2,2,2)
l1, = plt.plot(x, y, 'g-', label = 'spline 11.')
l2, = plt.plot(x, y1, 'b-', label = 'y=sin(x)')
l3, = plt.plot(x, y2, 'k-', label = 'spline 21.')
plt.legend(handles=[l1, l2, l3],labels=['spline 11', 'y=sin(x)', 'spline 21.'],loc='best')  
plt.subplot(2,2,3)
l1, = plt.plot(x, y, 'g-', label = 'spline 11.')
l2, = plt.plot(x, y1, 'b-', label = 'y=sin(x)')
l3, = plt.plot(x, y2, 'k-', label = 'spline 21.')
plt.legend(handles=[l1, l2, l3],labels=['spline 11', 'y=sin(x)', 'spline 21.'],loc='best')  
plt.subplot(2,2,4)
l1, = plt.plot(x, y, 'g-', label = 'spline 11.')
l2, = plt.plot(x, y1, 'b-', label = 'y=sin(x)')
l3, = plt.plot(x, y2, 'k-', label = 'spline 21.')
plt.legend(handles=[l1, l2, l3],labels=['spline 11', 'y=sin(x)', 'spline 21.'],loc='best')  
plt.show()  