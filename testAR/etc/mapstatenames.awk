BEGIN { 
statenamemap[1] = 1;
statename[1] = "*";
statenamemap[2] = 2;
statename[2] = "*";
statenamemap[3] = 3;
statename[3] = "*";
statenamemap[6] = 1;
statename[6] = "<";
statenamemap[7] = 2;
statename[7] = "<";
statenamemap[8] = 3;
statename[8] = "<";
statenamemap[11] = 1;
statename[11] = "D";
statenamemap[12] = 2;
statename[12] = "D";
statenamemap[13] = 3;
statename[13] = "D";
statenamemap[16] = 1;
statename[16] = "E";
statenamemap[17] = 2;
statename[17] = "E";
statenamemap[18] = 3;
statename[18] = "E";
statenamemap[21] = 1;
statename[21] = "H";
statenamemap[22] = 2;
statename[22] = "H";
statenamemap[23] = 3;
statename[23] = "H";
statenamemap[26] = 1;
statename[26] = "SS";
statenamemap[27] = 2;
statename[27] = "SS";
statenamemap[28] = 3;
statename[28] = "SS";
statenamemap[31] = 1;
statename[31] = "T";
statenamemap[32] = 2;
statename[32] = "T";
statenamemap[33] = 3;
statename[33] = "T";
statenamemap[36] = 1;
statename[36] = "^";
statenamemap[37] = 2;
statename[37] = "^";
statenamemap[38] = 3;
statename[38] = "^";
statenamemap[41] = 1;
statename[41] = "a";
statenamemap[42] = 2;
statename[42] = "a";
statenamemap[43] = 3;
statename[43] = "a";
statenamemap[46] = 1;
statename[46] = "aa";
statenamemap[47] = 2;
statename[47] = "aa";
statenamemap[48] = 3;
statename[48] = "aa";
statenamemap[51] = 1;
statename[51] = "b";
statenamemap[52] = 2;
statename[52] = "b";
statenamemap[53] = 3;
statename[53] = "b";
statenamemap[56] = 1;
statename[56] = "d";
statenamemap[57] = 2;
statename[57] = "d";
statenamemap[58] = 3;
statename[58] = "d";
statenamemap[61] = 1;
statename[61] = "dd";
statenamemap[62] = 2;
statename[62] = "dd";
statenamemap[63] = 3;
statename[63] = "dd";
statenamemap[66] = 1;
statename[66] = "f";
statenamemap[67] = 2;
statename[67] = "f";
statenamemap[68] = 3;
statename[68] = "f";
statenamemap[71] = 1;
statename[71] = "h";
statenamemap[72] = 2;
statename[72] = "h";
statenamemap[73] = 3;
statename[73] = "h";
statenamemap[76] = 1;
statename[76] = "i";
statenamemap[77] = 2;
statename[77] = "i";
statenamemap[78] = 3;
statename[78] = "i";
statenamemap[81] = 1;
statename[81] = "j";
statenamemap[82] = 2;
statename[82] = "j";
statenamemap[83] = 3;
statename[83] = "j";
statenamemap[86] = 1;
statename[86] = "jj";
statenamemap[87] = 2;
statename[87] = "jj";
statenamemap[88] = 3;
statename[88] = "jj";
statenamemap[91] = 1;
statename[91] = "k";
statenamemap[92] = 2;
statename[92] = "k";
statenamemap[93] = 3;
statename[93] = "k";
statenamemap[96] = 1;
statename[96] = "l";
statenamemap[97] = 2;
statename[97] = "l";
statenamemap[98] = 3;
statename[98] = "l";
statenamemap[101] = 1;
statename[101] = "ll";
statenamemap[102] = 2;
statename[102] = "ll";
statenamemap[103] = 3;
statename[103] = "ll";
statenamemap[106] = 1;
statename[106] = "m";
statenamemap[107] = 2;
statename[107] = "m";
statenamemap[108] = 3;
statename[108] = "m";
statenamemap[111] = 1;
statename[111] = "n";
statenamemap[112] = 2;
statename[112] = "n";
statenamemap[113] = 3;
statename[113] = "n";
statenamemap[116] = 1;
statename[116] = "pau";
statenamemap[117] = 2;
statename[117] = "pau";
statenamemap[118] = 3;
statename[118] = "pau";
statenamemap[121] = 1;
statename[121] = "q";
statenamemap[122] = 2;
statename[122] = "q";
statenamemap[123] = 3;
statename[123] = "q";
statenamemap[126] = 1;
statename[126] = "r";
statenamemap[127] = 2;
statename[127] = "r";
statenamemap[128] = 3;
statename[128] = "r";
statenamemap[131] = 1;
statename[131] = "rr";
statenamemap[132] = 2;
statename[132] = "rr";
statenamemap[133] = 3;
statename[133] = "rr";
statenamemap[136] = 1;
statename[136] = "s";
statenamemap[137] = 2;
statename[137] = "s";
statenamemap[138] = 3;
statename[138] = "s";
statenamemap[141] = 5;
statename[141] = "pau";
statenamemap[144] = 1;
statename[144] = "t";
statenamemap[145] = 2;
statename[145] = "t";
statenamemap[146] = 3;
statename[146] = "t";
statenamemap[149] = 1;
statename[149] = "tt";
statenamemap[150] = 2;
statename[150] = "tt";
statenamemap[151] = 3;
statename[151] = "tt";
statenamemap[154] = 1;
statename[154] = "u";
statenamemap[155] = 2;
statename[155] = "u";
statenamemap[156] = 3;
statename[156] = "u";
statenamemap[159] = 1;
statename[159] = "w";
statenamemap[160] = 2;
statename[160] = "w";
statenamemap[161] = 3;
statename[161] = "w";
statenamemap[164] = 1;
statename[164] = "y";
statenamemap[165] = 2;
statename[165] = "y";
statenamemap[166] = 3;
statename[166] = "y";
statenamemap[169] = 1;
statename[169] = "yy";
statenamemap[170] = 2;
statename[170] = "yy";
statenamemap[171] = 3;
statename[171] = "yy";
}
{ if (NF < 3)
     print $0;
  else
     printf("%s 125 %d %s\n",$1,statenamemap[$3],statename[$3])
}
