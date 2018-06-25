findOutside :: [Int] -> Int -> Int -> Int
findOutside list count location =
  let value = list!!location;
      increment = if value >= 3 then -1 else 1
  in
    if value + location >= length(list) then count + 1
    else if value + location < 0 then count + 1
    else findOutside ((take(location) list) ++ [value + increment] ++ drop (location + 1) list) (count + 1) (location + value)

main :: IO()
main =
  let input = [0, 2, 2, -2, -2, -2, 1, 2, 2, -1, -4, -8, -7, -11, 0, -13, -8, -7, -13, -11, -15, -4, -10, -10, -22, -22, -1, -9, 1, 2, -23, -4, -31, -3, 2, -34, -28, -28, -16, -1, -34, -13, -25, 1, -14, -40, -11, -32, -25, -17, -43, -23, -3, -52, -31, -8, -15, -48, -13, -56, -37, -24, -25, -47, -38, 0, -35, -65, -63, -40, -18, -55, -11, -8, -18, -53, -39, -20, 0, -6, -75, -22, -36, -71, -61, -74, -11, -14, -35, -54, -41, -59, -51, -45, -62, -53, -8, -26, -22, -16, -66, -87, -11, -59, -9, -44, -73, -76, -3, -78, -4, -45, -10, -38, -20, -102, -114, -44, -21, -111, -118, 0, -80, -65, -28, -51, -95, -42, -31, -41, -98, -39, -89, -116, -115, -30, -68, -52, -21, -17, -92, -136, -24, -16, -13, -110, -10, -44, 1, -89, -110, -99, -4, -106, -35, -7, -152, -147, -38, -17, -44, -148, -144, -48, -78, -154, -141, -23, -145, -55, -87, -97, -20, -66, -172, -82, -23, -23, -145, -37, -103, -138, -111, -105, -148, -67, -163, -59, -127, -130, -24, -102, -153, -149, -58, -33, -37, -28, -75, -103, -10, 1, -36, -130, -59, -70, -76, -181, -196, -165, -131, -71, -142, -183, -65, -55, -50, -112, -153, -59, -35, -198, -175, -197, -89, -197, -99, -52, -187, -55, -158, -117, -164, -205, -91, -68, -126, -158, -172, -217, -111, -106, -42, -135, -82, -213, -22, -1, -238, -204, -77, -123, -174, -29, -30, -90, -98, -5, -30, -52, -150, -155, -23, -14, -102, -47, -215, -112, -51, -2, 0, -62, -138, -255, -227, -17, -114, -34, -28, -139, -226, -258, -18, 0, -24, -161, -170, -12, -25, -12, -122, -238, -249, -81, -267, -236, -145, -242, -124, -78, -122, -141, -135, -9, -195, -98, -269, -221, -154, -106, -247, -88, -221, -9, -97, -207, -61, -102, -130, -54, -112, -61, -65, -82, -35, -150, -114, -180, -314, -317, -305, -28, -305, -294, -90, -133, -11, -1, -108, -1, -268, -257, -117, -245, -94, -313, -293, -174, -312, -61, -205, -3, -199, -95, -323, -34, -176, -241, -89, -346, -208, -270, -77, -253, -86, -167, -318, -70, -258, -76, -203, -91, -62, -329, -156, -235, -239, -191, -119, -285, -128, -155, -82, -36, -351, -373, -8, -36, -132, -183, -311, -274, -111, -175, -193, -72, -340, -64, -26, -378, -202, -194, -188, -169, -71, -197, -344, -193, -6, -63, -368, -313, -376, -42, -241, -50, -64, -354, -338, -177, -154, -420, -418, -349, -383, -340, -177, -227, -332, -145, -402, -66, -290, -274, -287, -68, -91, -49, -312, -106, -264, -173, -362, -252, -138, -218, -211, -39, -271, -261, -306, -372, -391, -408, -108, -308, -418, -395, -413, -208, -13, -41, -249, -297, -21, -274, -440, -205, -272, -371, -155, -357, -34, -97, -121, -221, -173, -364, -168, -83, -317, -414, -427, -90, -216, -91, -306, -467, -366, -486, -461, -34, -327, -474, -164, -160, -410, -366, -467, -206, -435, -167, -326, -423, -241, -45, -18, -59, -498, -483, -380, -431, -256, -254, -415, -18, -461, -223, -152, -179, -8, -169, -41, -452, -302, -172, -249, -5, -40, -97, -39, -492, -40, -460, -276, -442, -413, -220, -376, -389, -446, -133, -228, -364, -215, -133, -304, -234, -275, -463, -267, -147, -162, -227, -532, -216, -479, -61, -409, -376, -389, -283, -332, -97, -66, -486, -223, -522, -359, -340, -211, -401, -98, -249, -511, -238, -72, -473, -132, -450, -321, -455, -451, -140, -495, -212, -235, -213, -22, -111, -236, -390, -104, -230, -569, -45, -43, -264, -561, -552, -32, -403, -17, -288, -255, -436, -363, -495, -19, -358, -250, -59, -172, -77, -558, -211, -555, -65, -419, -282, -472, -72, -462, -276, -508, -489, -184, -486, -500, -143, -627, -506, -84, -368, -355, -467, -515, -415, -314, -386, -114, -319, -237, -113, -611, -17, -532, -181, -568, -132, -178, -276, -258, -384, -295, -208, -471, -622, -173, -531, -192, -231, -338, -427, -416, -591, -112, -511, -538, -357, -189, -186, -100, -458, -331, -85, -294, -375, -206, -377, -92, -504, -558, -26, -73, -103, -455, -397, -164, -683, -615, -321, -310, -76, -576, -291, -523, -163, -452, -236, -488, -588, -24, -52, -673, -176, -282, -642, -668, -517, -575, -173, -517, -565, -385, -202, -587, -519, -694, -493, -181, -241, -630, -597, -377, -560, -646, -100, -544, -135, -695, -460, -621, -97, -376, -511, -413, -613, -238, -160, -141, -329, -682, -664, -439, -730, -646, -266, -477, -233, -722, -687, -605, -483, -648, -685, -735, -739, -630, -644, -498, -652, -493, -508, -108, -32, -620, -185, -422, -187, -112, -263, -568, -599, -751, -768, -640, -440, -451, -760, -156, -425, -662, -764, -503, -521, -140, -425, -72, -242, -403, -778, -689, -693, -541, -674, -93, -545, -601, -730, -305, -743, -563, -315, -637, -126, -260, -463, -309, -538, -59, -368, -382, -355, -414, -97, -93, -475, -445, -319, -783, -411, -338, -480, -755, -149, -821, -813, -229, -116, -488, -741, -669, -442, -627, -403, -129, -829, -664, -357, -686, -835, -172, -80, -367, -789, -573, -199, -795, -221, -177, -543, -117, -651, -382, -731, -842, -125, -397, -328, -69, -388, -381, 1, -737, -199, -181, -264, -577, -63, -616, -333, -413, -616, -189, -315, -237, -608, -497, -348, -285, -863, -97, -745, -716, -666, -165, -522, -631, -438, -639, -443, -122, -521, -48, -501, -895, -205, -57, -576, -644, -442, -143, -215, -661, -749, -247, -298, -387, -601, -525, -383, -9, -64, -416, -423, -522, -631, -890, -867, -649, -525, -228, -544, -54, -878, -277, -924, -838, -885, -477, -256, -330, -301, -815, -722, -646, -677, -70, -917, -126, -832, -479, -849, -591, -66, -260, -524, -603, -86, -397, -63, -299, -417, -375, -909, -489, -872, -930, -638, -280, -440, -788, -818, -398, -765, -229, -346, -864, -155, -57, -686, -850, -84, -783, -191, -923, -740, -454, -118, -807, -662, -859, -99, -139, -272, -640, -166, -935, -805, -351, -413, -467, -535, -377, -97, -204, -262, -4, -704, -516, -459, -702, -718, -241, -534, -318, -955, -519, -675, -766, -671, -843, -861, -214, -4, -828, -638, -833, -953, -521, -17, -87, -393, -951, -17, -529, -49, -299, -673, -119, -185, -601, -187, -399, -646, -812, -627, -121, -535, -155, -601, -196, -365, -366, -409, -596, -803, -508, -988, -529, -925] in
    print $ show $ findOutside input 0 0
