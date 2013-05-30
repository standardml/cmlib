
structure AES 
   :>
   CIPHER
   where type prekey = Bytestring.string
   where type init = unit
   =
   struct

      structure A = Word8Array
      structure B = Bytestring

      val w2i = Word8.toInt
      val xorb = Word8.xorb
      val sub = Word8Vector.sub
      val asub = Word8Array.sub
      val bsub = Bytestring.sub

      val rcon =
         Word8Vector.fromList
         [0wx8D,0wx1,0wx2,0wx4,0wx8,0wx10,0wx20,0wx40,0wx80,0wx1B,0wx36,0wx6C,0wxD8,
          0wxAB,0wx4D,0wx9A,0wx2F,0wx5E,0wxBC,0wx63,0wxC6,0wx97,0wx35,0wx6A,0wxD4,
          0wxB3,0wx7D,0wxFA,0wxEF,0wxC5,0wx91,0wx39,0wx72,0wxE4,0wxD3,0wxBD,0wx61,
          0wxC2,0wx9F,0wx25,0wx4A,0wx94,0wx33,0wx66,0wxCC,0wx83,0wx1D,0wx3A,0wx74,
          0wxE8,0wxCB,0wx8D,0wx1,0wx2,0wx4,0wx8,0wx10,0wx20,0wx40,0wx80,0wx1B,0wx36,
          0wx6C,0wxD8,0wxAB,0wx4D,0wx9A,0wx2F,0wx5E,0wxBC,0wx63,0wxC6,0wx97,0wx35,
          0wx6A,0wxD4,0wxB3,0wx7D,0wxFA,0wxEF,0wxC5,0wx91,0wx39,0wx72,0wxE4,0wxD3,
          0wxBD,0wx61,0wxC2,0wx9F,0wx25,0wx4A,0wx94,0wx33,0wx66,0wxCC,0wx83,0wx1D,
          0wx3A,0wx74,0wxE8,0wxCB,0wx8D,0wx1,0wx2,0wx4,0wx8,0wx10,0wx20,0wx40,0wx80,
          0wx1B,0wx36,0wx6C,0wxD8,0wxAB,0wx4D,0wx9A,0wx2F,0wx5E,0wxBC,0wx63,0wxC6,
          0wx97,0wx35,0wx6A,0wxD4,0wxB3,0wx7D,0wxFA,0wxEF,0wxC5,0wx91,0wx39,0wx72,
          0wxE4,0wxD3,0wxBD,0wx61,0wxC2,0wx9F,0wx25,0wx4A,0wx94,0wx33,0wx66,0wxCC,
          0wx83,0wx1D,0wx3A,0wx74,0wxE8,0wxCB,0wx8D,0wx1,0wx2,0wx4,0wx8,0wx10,0wx20,
          0wx40,0wx80,0wx1B,0wx36,0wx6C,0wxD8,0wxAB,0wx4D,0wx9A,0wx2F,0wx5E,0wxBC,
          0wx63,0wxC6,0wx97,0wx35,0wx6A,0wxD4,0wxB3,0wx7D,0wxFA,0wxEF,0wxC5,0wx91,
          0wx39,0wx72,0wxE4,0wxD3,0wxBD,0wx61,0wxC2,0wx9F,0wx25,0wx4A,0wx94,0wx33,
          0wx66,0wxCC,0wx83,0wx1D,0wx3A,0wx74,0wxE8,0wxCB,0wx8D,0wx1,0wx2,0wx4,0wx8,
          0wx10,0wx20,0wx40,0wx80,0wx1B,0wx36,0wx6C,0wxD8,0wxAB,0wx4D,0wx9A,0wx2F,
          0wx5E,0wxBC,0wx63,0wxC6,0wx97,0wx35,0wx6A,0wxD4,0wxB3,0wx7D,0wxFA,0wxEF,
          0wxC5,0wx91,0wx39,0wx72,0wxE4,0wxD3,0wxBD,0wx61,0wxC2,0wx9F,0wx25,0wx4A,
          0wx94,0wx33,0wx66,0wxCC,0wx83,0wx1D,0wx3A,0wx74,0wxE8,0wxCB,0wx8D]
      
      val sbox =
         Word8Vector.fromList
         [0wx63,0wx7C,0wx77,0wx7B,0wxF2,0wx6B,0wx6F,0wxC5,0wx30,0wx1,0wx67,0wx2B,
          0wxFE,0wxD7,0wxAB,0wx76,0wxCA,0wx82,0wxC9,0wx7D,0wxFA,0wx59,0wx47,0wxF0,
          0wxAD,0wxD4,0wxA2,0wxAF,0wx9C,0wxA4,0wx72,0wxC0,0wxB7,0wxFD,0wx93,0wx26,
          0wx36,0wx3F,0wxF7,0wxCC,0wx34,0wxA5,0wxE5,0wxF1,0wx71,0wxD8,0wx31,0wx15,
          0wx4,0wxC7,0wx23,0wxC3,0wx18,0wx96,0wx5,0wx9A,0wx7,0wx12,0wx80,0wxE2,0wxEB,
          0wx27,0wxB2,0wx75,0wx9,0wx83,0wx2C,0wx1A,0wx1B,0wx6E,0wx5A,0wxA0,0wx52,
          0wx3B,0wxD6,0wxB3,0wx29,0wxE3,0wx2F,0wx84,0wx53,0wxD1,0wx0,0wxED,0wx20,
          0wxFC,0wxB1,0wx5B,0wx6A,0wxCB,0wxBE,0wx39,0wx4A,0wx4C,0wx58,0wxCF,0wxD0,
          0wxEF,0wxAA,0wxFB,0wx43,0wx4D,0wx33,0wx85,0wx45,0wxF9,0wx2,0wx7F,0wx50,
          0wx3C,0wx9F,0wxA8,0wx51,0wxA3,0wx40,0wx8F,0wx92,0wx9D,0wx38,0wxF5,0wxBC,
          0wxB6,0wxDA,0wx21,0wx10,0wxFF,0wxF3,0wxD2,0wxCD,0wxC,0wx13,0wxEC,0wx5F,
          0wx97,0wx44,0wx17,0wxC4,0wxA7,0wx7E,0wx3D,0wx64,0wx5D,0wx19,0wx73,0wx60,
          0wx81,0wx4F,0wxDC,0wx22,0wx2A,0wx90,0wx88,0wx46,0wxEE,0wxB8,0wx14,0wxDE,
          0wx5E,0wxB,0wxDB,0wxE0,0wx32,0wx3A,0wxA,0wx49,0wx6,0wx24,0wx5C,0wxC2,0wxD3,
          0wxAC,0wx62,0wx91,0wx95,0wxE4,0wx79,0wxE7,0wxC8,0wx37,0wx6D,0wx8D,0wxD5,
          0wx4E,0wxA9,0wx6C,0wx56,0wxF4,0wxEA,0wx65,0wx7A,0wxAE,0wx8,0wxBA,0wx78,
          0wx25,0wx2E,0wx1C,0wxA6,0wxB4,0wxC6,0wxE8,0wxDD,0wx74,0wx1F,0wx4B,0wxBD,
          0wx8B,0wx8A,0wx70,0wx3E,0wxB5,0wx66,0wx48,0wx3,0wxF6,0wxE,0wx61,0wx35,
          0wx57,0wxB9,0wx86,0wxC1,0wx1D,0wx9E,0wxE1,0wxF8,0wx98,0wx11,0wx69,0wxD9,
          0wx8E,0wx94,0wx9B,0wx1E,0wx87,0wxE9,0wxCE,0wx55,0wx28,0wxDF,0wx8C,0wxA1,
          0wx89,0wxD,0wxBF,0wxE6,0wx42,0wx68,0wx41,0wx99,0wx2D,0wxF,0wxB0,0wx54,
          0wxBB,0wx16]

      val invSbox =
         Word8Vector.fromList
         [0wx52,0wx9,0wx6A,0wxD5,0wx30,0wx36,0wxA5,0wx38,0wxBF,0wx40,0wxA3,0wx9E,
          0wx81,0wxF3,0wxD7,0wxFB,0wx7C,0wxE3,0wx39,0wx82,0wx9B,0wx2F,0wxFF,0wx87,
          0wx34,0wx8E,0wx43,0wx44,0wxC4,0wxDE,0wxE9,0wxCB,0wx54,0wx7B,0wx94,0wx32,
          0wxA6,0wxC2,0wx23,0wx3D,0wxEE,0wx4C,0wx95,0wxB,0wx42,0wxFA,0wxC3,0wx4E,
          0wx8,0wx2E,0wxA1,0wx66,0wx28,0wxD9,0wx24,0wxB2,0wx76,0wx5B,0wxA2,0wx49,
          0wx6D,0wx8B,0wxD1,0wx25,0wx72,0wxF8,0wxF6,0wx64,0wx86,0wx68,0wx98,0wx16,
          0wxD4,0wxA4,0wx5C,0wxCC,0wx5D,0wx65,0wxB6,0wx92,0wx6C,0wx70,0wx48,0wx50,
          0wxFD,0wxED,0wxB9,0wxDA,0wx5E,0wx15,0wx46,0wx57,0wxA7,0wx8D,0wx9D,0wx84,
          0wx90,0wxD8,0wxAB,0wx0,0wx8C,0wxBC,0wxD3,0wxA,0wxF7,0wxE4,0wx58,0wx5,0wxB8,
          0wxB3,0wx45,0wx6,0wxD0,0wx2C,0wx1E,0wx8F,0wxCA,0wx3F,0wxF,0wx2,0wxC1,0wxAF,
          0wxBD,0wx3,0wx1,0wx13,0wx8A,0wx6B,0wx3A,0wx91,0wx11,0wx41,0wx4F,0wx67,
          0wxDC,0wxEA,0wx97,0wxF2,0wxCF,0wxCE,0wxF0,0wxB4,0wxE6,0wx73,0wx96,0wxAC,
          0wx74,0wx22,0wxE7,0wxAD,0wx35,0wx85,0wxE2,0wxF9,0wx37,0wxE8,0wx1C,0wx75,
          0wxDF,0wx6E,0wx47,0wxF1,0wx1A,0wx71,0wx1D,0wx29,0wxC5,0wx89,0wx6F,0wxB7,
          0wx62,0wxE,0wxAA,0wx18,0wxBE,0wx1B,0wxFC,0wx56,0wx3E,0wx4B,0wxC6,0wxD2,
          0wx79,0wx20,0wx9A,0wxDB,0wxC0,0wxFE,0wx78,0wxCD,0wx5A,0wxF4,0wx1F,0wxDD,
          0wxA8,0wx33,0wx88,0wx7,0wxC7,0wx31,0wxB1,0wx12,0wx10,0wx59,0wx27,0wx80,
          0wxEC,0wx5F,0wx60,0wx51,0wx7F,0wxA9,0wx19,0wxB5,0wx4A,0wxD,0wx2D,0wxE5,
          0wx7A,0wx9F,0wx93,0wxC9,0wx9C,0wxEF,0wxA0,0wxE0,0wx3B,0wx4D,0wxAE,0wx2A,
          0wxF5,0wxB0,0wxC8,0wxEB,0wxBB,0wx3C,0wx83,0wx53,0wx99,0wx61,0wx17,0wx2B,
          0wx4,0wx7E,0wxBA,0wx77,0wxD6,0wx26,0wxE1,0wx69,0wx14,0wx63,0wx55,0wx21,
          0wxC,0wx7D]

      val times2 =
         Word8Vector.fromList
         [0wx0,0wx2,0wx4,0wx6,0wx8,0wxA,0wxC,0wxE,0wx10,0wx12,0wx14,0wx16,0wx18,
          0wx1A,0wx1C,0wx1E,0wx20,0wx22,0wx24,0wx26,0wx28,0wx2A,0wx2C,0wx2E,0wx30,
          0wx32,0wx34,0wx36,0wx38,0wx3A,0wx3C,0wx3E,0wx40,0wx42,0wx44,0wx46,0wx48,
          0wx4A,0wx4C,0wx4E,0wx50,0wx52,0wx54,0wx56,0wx58,0wx5A,0wx5C,0wx5E,0wx60,
          0wx62,0wx64,0wx66,0wx68,0wx6A,0wx6C,0wx6E,0wx70,0wx72,0wx74,0wx76,0wx78,
          0wx7A,0wx7C,0wx7E,0wx80,0wx82,0wx84,0wx86,0wx88,0wx8A,0wx8C,0wx8E,0wx90,
          0wx92,0wx94,0wx96,0wx98,0wx9A,0wx9C,0wx9E,0wxA0,0wxA2,0wxA4,0wxA6,0wxA8,
          0wxAA,0wxAC,0wxAE,0wxB0,0wxB2,0wxB4,0wxB6,0wxB8,0wxBA,0wxBC,0wxBE,0wxC0,
          0wxC2,0wxC4,0wxC6,0wxC8,0wxCA,0wxCC,0wxCE,0wxD0,0wxD2,0wxD4,0wxD6,0wxD8,
          0wxDA,0wxDC,0wxDE,0wxE0,0wxE2,0wxE4,0wxE6,0wxE8,0wxEA,0wxEC,0wxEE,0wxF0,
          0wxF2,0wxF4,0wxF6,0wxF8,0wxFA,0wxFC,0wxFE,0wx1B,0wx19,0wx1F,0wx1D,0wx13,
          0wx11,0wx17,0wx15,0wxB,0wx9,0wxF,0wxD,0wx3,0wx1,0wx7,0wx5,0wx3B,0wx39,
          0wx3F,0wx3D,0wx33,0wx31,0wx37,0wx35,0wx2B,0wx29,0wx2F,0wx2D,0wx23,0wx21,
          0wx27,0wx25,0wx5B,0wx59,0wx5F,0wx5D,0wx53,0wx51,0wx57,0wx55,0wx4B,0wx49,
          0wx4F,0wx4D,0wx43,0wx41,0wx47,0wx45,0wx7B,0wx79,0wx7F,0wx7D,0wx73,0wx71,
          0wx77,0wx75,0wx6B,0wx69,0wx6F,0wx6D,0wx63,0wx61,0wx67,0wx65,0wx9B,0wx99,
          0wx9F,0wx9D,0wx93,0wx91,0wx97,0wx95,0wx8B,0wx89,0wx8F,0wx8D,0wx83,0wx81,
          0wx87,0wx85,0wxBB,0wxB9,0wxBF,0wxBD,0wxB3,0wxB1,0wxB7,0wxB5,0wxAB,0wxA9,
          0wxAF,0wxAD,0wxA3,0wxA1,0wxA7,0wxA5,0wxDB,0wxD9,0wxDF,0wxDD,0wxD3,0wxD1,
          0wxD7,0wxD5,0wxCB,0wxC9,0wxCF,0wxCD,0wxC3,0wxC1,0wxC7,0wxC5,0wxFB,0wxF9,
          0wxFF,0wxFD,0wxF3,0wxF1,0wxF7,0wxF5,0wxEB,0wxE9,0wxEF,0wxED,0wxE3,0wxE1,
          0wxE7,0wxE5]

      val times3 =
         Word8Vector.fromList
         [0wx0,0wx3,0wx6,0wx5,0wxC,0wxF,0wxA,0wx9,0wx18,0wx1B,0wx1E,0wx1D,0wx14,
          0wx17,0wx12,0wx11,0wx30,0wx33,0wx36,0wx35,0wx3C,0wx3F,0wx3A,0wx39,0wx28,
          0wx2B,0wx2E,0wx2D,0wx24,0wx27,0wx22,0wx21,0wx60,0wx63,0wx66,0wx65,0wx6C,
          0wx6F,0wx6A,0wx69,0wx78,0wx7B,0wx7E,0wx7D,0wx74,0wx77,0wx72,0wx71,0wx50,
          0wx53,0wx56,0wx55,0wx5C,0wx5F,0wx5A,0wx59,0wx48,0wx4B,0wx4E,0wx4D,0wx44,
          0wx47,0wx42,0wx41,0wxC0,0wxC3,0wxC6,0wxC5,0wxCC,0wxCF,0wxCA,0wxC9,0wxD8,
          0wxDB,0wxDE,0wxDD,0wxD4,0wxD7,0wxD2,0wxD1,0wxF0,0wxF3,0wxF6,0wxF5,0wxFC,
          0wxFF,0wxFA,0wxF9,0wxE8,0wxEB,0wxEE,0wxED,0wxE4,0wxE7,0wxE2,0wxE1,0wxA0,
          0wxA3,0wxA6,0wxA5,0wxAC,0wxAF,0wxAA,0wxA9,0wxB8,0wxBB,0wxBE,0wxBD,0wxB4,
          0wxB7,0wxB2,0wxB1,0wx90,0wx93,0wx96,0wx95,0wx9C,0wx9F,0wx9A,0wx99,0wx88,
          0wx8B,0wx8E,0wx8D,0wx84,0wx87,0wx82,0wx81,0wx9B,0wx98,0wx9D,0wx9E,0wx97,
          0wx94,0wx91,0wx92,0wx83,0wx80,0wx85,0wx86,0wx8F,0wx8C,0wx89,0wx8A,0wxAB,
          0wxA8,0wxAD,0wxAE,0wxA7,0wxA4,0wxA1,0wxA2,0wxB3,0wxB0,0wxB5,0wxB6,0wxBF,
          0wxBC,0wxB9,0wxBA,0wxFB,0wxF8,0wxFD,0wxFE,0wxF7,0wxF4,0wxF1,0wxF2,0wxE3,
          0wxE0,0wxE5,0wxE6,0wxEF,0wxEC,0wxE9,0wxEA,0wxCB,0wxC8,0wxCD,0wxCE,0wxC7,
          0wxC4,0wxC1,0wxC2,0wxD3,0wxD0,0wxD5,0wxD6,0wxDF,0wxDC,0wxD9,0wxDA,0wx5B,
          0wx58,0wx5D,0wx5E,0wx57,0wx54,0wx51,0wx52,0wx43,0wx40,0wx45,0wx46,0wx4F,
          0wx4C,0wx49,0wx4A,0wx6B,0wx68,0wx6D,0wx6E,0wx67,0wx64,0wx61,0wx62,0wx73,
          0wx70,0wx75,0wx76,0wx7F,0wx7C,0wx79,0wx7A,0wx3B,0wx38,0wx3D,0wx3E,0wx37,
          0wx34,0wx31,0wx32,0wx23,0wx20,0wx25,0wx26,0wx2F,0wx2C,0wx29,0wx2A,0wxB,
          0wx8,0wxD,0wxE,0wx7,0wx4,0wx1,0wx2,0wx13,0wx10,0wx15,0wx16,0wx1F,0wx1C,
          0wx19,0wx1A]         

      val times9 =
         Word8Vector.fromList
         [0wx0,0wx9,0wx12,0wx1B,0wx24,0wx2D,0wx36,0wx3F,0wx48,0wx41,0wx5A,0wx53,
          0wx6C,0wx65,0wx7E,0wx77,0wx90,0wx99,0wx82,0wx8B,0wxB4,0wxBD,0wxA6,0wxAF,
          0wxD8,0wxD1,0wxCA,0wxC3,0wxFC,0wxF5,0wxEE,0wxE7,0wx3B,0wx32,0wx29,0wx20,
          0wx1F,0wx16,0wxD,0wx4,0wx73,0wx7A,0wx61,0wx68,0wx57,0wx5E,0wx45,0wx4C,
          0wxAB,0wxA2,0wxB9,0wxB0,0wx8F,0wx86,0wx9D,0wx94,0wxE3,0wxEA,0wxF1,0wxF8,
          0wxC7,0wxCE,0wxD5,0wxDC,0wx76,0wx7F,0wx64,0wx6D,0wx52,0wx5B,0wx40,0wx49,
          0wx3E,0wx37,0wx2C,0wx25,0wx1A,0wx13,0wx8,0wx1,0wxE6,0wxEF,0wxF4,0wxFD,
          0wxC2,0wxCB,0wxD0,0wxD9,0wxAE,0wxA7,0wxBC,0wxB5,0wx8A,0wx83,0wx98,0wx91,
          0wx4D,0wx44,0wx5F,0wx56,0wx69,0wx60,0wx7B,0wx72,0wx5,0wxC,0wx17,0wx1E,
          0wx21,0wx28,0wx33,0wx3A,0wxDD,0wxD4,0wxCF,0wxC6,0wxF9,0wxF0,0wxEB,0wxE2,
          0wx95,0wx9C,0wx87,0wx8E,0wxB1,0wxB8,0wxA3,0wxAA,0wxEC,0wxE5,0wxFE,0wxF7,
          0wxC8,0wxC1,0wxDA,0wxD3,0wxA4,0wxAD,0wxB6,0wxBF,0wx80,0wx89,0wx92,0wx9B,
          0wx7C,0wx75,0wx6E,0wx67,0wx58,0wx51,0wx4A,0wx43,0wx34,0wx3D,0wx26,0wx2F,
          0wx10,0wx19,0wx2,0wxB,0wxD7,0wxDE,0wxC5,0wxCC,0wxF3,0wxFA,0wxE1,0wxE8,
          0wx9F,0wx96,0wx8D,0wx84,0wxBB,0wxB2,0wxA9,0wxA0,0wx47,0wx4E,0wx55,0wx5C,
          0wx63,0wx6A,0wx71,0wx78,0wxF,0wx6,0wx1D,0wx14,0wx2B,0wx22,0wx39,0wx30,
          0wx9A,0wx93,0wx88,0wx81,0wxBE,0wxB7,0wxAC,0wxA5,0wxD2,0wxDB,0wxC0,0wxC9,
          0wxF6,0wxFF,0wxE4,0wxED,0wxA,0wx3,0wx18,0wx11,0wx2E,0wx27,0wx3C,0wx35,
          0wx42,0wx4B,0wx50,0wx59,0wx66,0wx6F,0wx74,0wx7D,0wxA1,0wxA8,0wxB3,0wxBA,
          0wx85,0wx8C,0wx97,0wx9E,0wxE9,0wxE0,0wxFB,0wxF2,0wxCD,0wxC4,0wxDF,0wxD6,
          0wx31,0wx38,0wx23,0wx2A,0wx15,0wx1C,0wx7,0wxE,0wx79,0wx70,0wx6B,0wx62,
          0wx5D,0wx54,0wx4F,0wx46]

      val times11 =
         Word8Vector.fromList
         [0wx0,0wxB,0wx16,0wx1D,0wx2C,0wx27,0wx3A,0wx31,0wx58,0wx53,0wx4E,0wx45,
          0wx74,0wx7F,0wx62,0wx69,0wxB0,0wxBB,0wxA6,0wxAD,0wx9C,0wx97,0wx8A,0wx81,
          0wxE8,0wxE3,0wxFE,0wxF5,0wxC4,0wxCF,0wxD2,0wxD9,0wx7B,0wx70,0wx6D,0wx66,
          0wx57,0wx5C,0wx41,0wx4A,0wx23,0wx28,0wx35,0wx3E,0wxF,0wx4,0wx19,0wx12,
          0wxCB,0wxC0,0wxDD,0wxD6,0wxE7,0wxEC,0wxF1,0wxFA,0wx93,0wx98,0wx85,0wx8E,
          0wxBF,0wxB4,0wxA9,0wxA2,0wxF6,0wxFD,0wxE0,0wxEB,0wxDA,0wxD1,0wxCC,0wxC7,
          0wxAE,0wxA5,0wxB8,0wxB3,0wx82,0wx89,0wx94,0wx9F,0wx46,0wx4D,0wx50,0wx5B,
          0wx6A,0wx61,0wx7C,0wx77,0wx1E,0wx15,0wx8,0wx3,0wx32,0wx39,0wx24,0wx2F,
          0wx8D,0wx86,0wx9B,0wx90,0wxA1,0wxAA,0wxB7,0wxBC,0wxD5,0wxDE,0wxC3,0wxC8,
          0wxF9,0wxF2,0wxEF,0wxE4,0wx3D,0wx36,0wx2B,0wx20,0wx11,0wx1A,0wx7,0wxC,
          0wx65,0wx6E,0wx73,0wx78,0wx49,0wx42,0wx5F,0wx54,0wxF7,0wxFC,0wxE1,0wxEA,
          0wxDB,0wxD0,0wxCD,0wxC6,0wxAF,0wxA4,0wxB9,0wxB2,0wx83,0wx88,0wx95,0wx9E,
          0wx47,0wx4C,0wx51,0wx5A,0wx6B,0wx60,0wx7D,0wx76,0wx1F,0wx14,0wx9,0wx2,
          0wx33,0wx38,0wx25,0wx2E,0wx8C,0wx87,0wx9A,0wx91,0wxA0,0wxAB,0wxB6,0wxBD,
          0wxD4,0wxDF,0wxC2,0wxC9,0wxF8,0wxF3,0wxEE,0wxE5,0wx3C,0wx37,0wx2A,0wx21,
          0wx10,0wx1B,0wx6,0wxD,0wx64,0wx6F,0wx72,0wx79,0wx48,0wx43,0wx5E,0wx55,0wx1,
          0wxA,0wx17,0wx1C,0wx2D,0wx26,0wx3B,0wx30,0wx59,0wx52,0wx4F,0wx44,0wx75,
          0wx7E,0wx63,0wx68,0wxB1,0wxBA,0wxA7,0wxAC,0wx9D,0wx96,0wx8B,0wx80,0wxE9,
          0wxE2,0wxFF,0wxF4,0wxC5,0wxCE,0wxD3,0wxD8,0wx7A,0wx71,0wx6C,0wx67,0wx56,
          0wx5D,0wx40,0wx4B,0wx22,0wx29,0wx34,0wx3F,0wxE,0wx5,0wx18,0wx13,0wxCA,
          0wxC1,0wxDC,0wxD7,0wxE6,0wxED,0wxF0,0wxFB,0wx92,0wx99,0wx84,0wx8F,0wxBE,
          0wxB5,0wxA8,0wxA3]
       
      val times13 =
         Word8Vector.fromList
         [0wx0,0wxD,0wx1A,0wx17,0wx34,0wx39,0wx2E,0wx23,0wx68,0wx65,0wx72,0wx7F,
          0wx5C,0wx51,0wx46,0wx4B,0wxD0,0wxDD,0wxCA,0wxC7,0wxE4,0wxE9,0wxFE,0wxF3,
          0wxB8,0wxB5,0wxA2,0wxAF,0wx8C,0wx81,0wx96,0wx9B,0wxBB,0wxB6,0wxA1,0wxAC,
          0wx8F,0wx82,0wx95,0wx98,0wxD3,0wxDE,0wxC9,0wxC4,0wxE7,0wxEA,0wxFD,0wxF0,
          0wx6B,0wx66,0wx71,0wx7C,0wx5F,0wx52,0wx45,0wx48,0wx3,0wxE,0wx19,0wx14,
          0wx37,0wx3A,0wx2D,0wx20,0wx6D,0wx60,0wx77,0wx7A,0wx59,0wx54,0wx43,0wx4E,
          0wx5,0wx8,0wx1F,0wx12,0wx31,0wx3C,0wx2B,0wx26,0wxBD,0wxB0,0wxA7,0wxAA,
          0wx89,0wx84,0wx93,0wx9E,0wxD5,0wxD8,0wxCF,0wxC2,0wxE1,0wxEC,0wxFB,0wxF6,
          0wxD6,0wxDB,0wxCC,0wxC1,0wxE2,0wxEF,0wxF8,0wxF5,0wxBE,0wxB3,0wxA4,0wxA9,
          0wx8A,0wx87,0wx90,0wx9D,0wx6,0wxB,0wx1C,0wx11,0wx32,0wx3F,0wx28,0wx25,
          0wx6E,0wx63,0wx74,0wx79,0wx5A,0wx57,0wx40,0wx4D,0wxDA,0wxD7,0wxC0,0wxCD,
          0wxEE,0wxE3,0wxF4,0wxF9,0wxB2,0wxBF,0wxA8,0wxA5,0wx86,0wx8B,0wx9C,0wx91,
          0wxA,0wx7,0wx10,0wx1D,0wx3E,0wx33,0wx24,0wx29,0wx62,0wx6F,0wx78,0wx75,
          0wx56,0wx5B,0wx4C,0wx41,0wx61,0wx6C,0wx7B,0wx76,0wx55,0wx58,0wx4F,0wx42,
          0wx9,0wx4,0wx13,0wx1E,0wx3D,0wx30,0wx27,0wx2A,0wxB1,0wxBC,0wxAB,0wxA6,
          0wx85,0wx88,0wx9F,0wx92,0wxD9,0wxD4,0wxC3,0wxCE,0wxED,0wxE0,0wxF7,0wxFA,
          0wxB7,0wxBA,0wxAD,0wxA0,0wx83,0wx8E,0wx99,0wx94,0wxDF,0wxD2,0wxC5,0wxC8,
          0wxEB,0wxE6,0wxF1,0wxFC,0wx67,0wx6A,0wx7D,0wx70,0wx53,0wx5E,0wx49,0wx44,
          0wxF,0wx2,0wx15,0wx18,0wx3B,0wx36,0wx21,0wx2C,0wxC,0wx1,0wx16,0wx1B,0wx38,
          0wx35,0wx22,0wx2F,0wx64,0wx69,0wx7E,0wx73,0wx50,0wx5D,0wx4A,0wx47,0wxDC,
          0wxD1,0wxC6,0wxCB,0wxE8,0wxE5,0wxF2,0wxFF,0wxB4,0wxB9,0wxAE,0wxA3,0wx80,
          0wx8D,0wx9A,0wx97]

      val times14 =
         Word8Vector.fromList
         [0wx0,0wxE,0wx1C,0wx12,0wx38,0wx36,0wx24,0wx2A,0wx70,0wx7E,0wx6C,0wx62,
          0wx48,0wx46,0wx54,0wx5A,0wxE0,0wxEE,0wxFC,0wxF2,0wxD8,0wxD6,0wxC4,0wxCA,
          0wx90,0wx9E,0wx8C,0wx82,0wxA8,0wxA6,0wxB4,0wxBA,0wxDB,0wxD5,0wxC7,0wxC9,
          0wxE3,0wxED,0wxFF,0wxF1,0wxAB,0wxA5,0wxB7,0wxB9,0wx93,0wx9D,0wx8F,0wx81,
          0wx3B,0wx35,0wx27,0wx29,0wx3,0wxD,0wx1F,0wx11,0wx4B,0wx45,0wx57,0wx59,
          0wx73,0wx7D,0wx6F,0wx61,0wxAD,0wxA3,0wxB1,0wxBF,0wx95,0wx9B,0wx89,0wx87,
          0wxDD,0wxD3,0wxC1,0wxCF,0wxE5,0wxEB,0wxF9,0wxF7,0wx4D,0wx43,0wx51,0wx5F,
          0wx75,0wx7B,0wx69,0wx67,0wx3D,0wx33,0wx21,0wx2F,0wx5,0wxB,0wx19,0wx17,
          0wx76,0wx78,0wx6A,0wx64,0wx4E,0wx40,0wx52,0wx5C,0wx6,0wx8,0wx1A,0wx14,
          0wx3E,0wx30,0wx22,0wx2C,0wx96,0wx98,0wx8A,0wx84,0wxAE,0wxA0,0wxB2,0wxBC,
          0wxE6,0wxE8,0wxFA,0wxF4,0wxDE,0wxD0,0wxC2,0wxCC,0wx41,0wx4F,0wx5D,0wx53,
          0wx79,0wx77,0wx65,0wx6B,0wx31,0wx3F,0wx2D,0wx23,0wx9,0wx7,0wx15,0wx1B,
          0wxA1,0wxAF,0wxBD,0wxB3,0wx99,0wx97,0wx85,0wx8B,0wxD1,0wxDF,0wxCD,0wxC3,
          0wxE9,0wxE7,0wxF5,0wxFB,0wx9A,0wx94,0wx86,0wx88,0wxA2,0wxAC,0wxBE,0wxB0,
          0wxEA,0wxE4,0wxF6,0wxF8,0wxD2,0wxDC,0wxCE,0wxC0,0wx7A,0wx74,0wx66,0wx68,
          0wx42,0wx4C,0wx5E,0wx50,0wxA,0wx4,0wx16,0wx18,0wx32,0wx3C,0wx2E,0wx20,
          0wxEC,0wxE2,0wxF0,0wxFE,0wxD4,0wxDA,0wxC8,0wxC6,0wx9C,0wx92,0wx80,0wx8E,
          0wxA4,0wxAA,0wxB8,0wxB6,0wxC,0wx2,0wx10,0wx1E,0wx34,0wx3A,0wx28,0wx26,
          0wx7C,0wx72,0wx60,0wx6E,0wx44,0wx4A,0wx58,0wx56,0wx37,0wx39,0wx2B,0wx25,
          0wxF,0wx1,0wx13,0wx1D,0wx47,0wx49,0wx5B,0wx55,0wx7F,0wx71,0wx63,0wx6D,
          0wxD7,0wxD9,0wxCB,0wxC5,0wxEF,0wxE1,0wxF3,0wxFD,0wxA7,0wxA9,0wxBB,0wxB5,
          0wx9F,0wx91,0wx83,0wx8D]

(*
      fun ksc (a, b, c, d, i) =
         (xorb (sub (sbox, w2i b), sub (rcon, i)),
          sub (sbox, w2i c),
          sub (sbox, w2i d),
          sub (sbox, w2i a))
*)

      fun mixColumns (a, b, c, d) =
         let
            val a' = w2i a
            val b' = w2i b
            val c' = w2i c
            val d' = w2i d
         in
            (xorb (xorb (xorb (sub (times2, a'), sub (times3, b')), c), d),
             xorb (xorb (xorb (a, sub (times2, b')), sub (times3, c')), d),
             xorb (xorb (xorb (a, b), sub (times2, c')), sub (times3, d')),
             xorb (xorb (xorb (sub (times3, a'), b), c), sub (times2, d')))
         end
         
      fun invMixColumns (a, b, c, d) =
         let
            val a' = w2i a
            val b' = w2i b
            val c' = w2i c
            val d' = w2i d
         in
            (xorb (xorb (xorb (sub (times14, a'), sub (times11, b')), sub (times13, c')), sub (times9, d')),
             xorb (xorb (xorb (sub (times9, a'), sub (times14, b')), sub (times11, c')), sub (times13, d')),
             xorb (xorb (xorb (sub (times13, a'), sub (times9, b')), sub (times14, c')), sub (times11, d')),
             xorb (xorb (xorb (sub (times11, a'), sub (times13, b')), sub (times9, c')), sub (times14, d')))
         end


      type prekey = Bytestring.string
      type key = Word8Array.array
      type init = unit


      val messageSize = SOME 16


      fun fullkey128 prekey =
         let
            val keyv = B.toWord8Vector prekey

            val fkey = Word8Array.array (176, 0w0)
            val () = Word8Array.copyVec {src=keyv, dst=fkey, di=0}

            fun loop1 (pos, i, t0, t1, t2, t3) =
               if pos >= 176 then
                  ()
               else
                  let
                     val t0' = xorb (sub (sbox, w2i t1), sub (rcon, i))
                     val t1' = sub (sbox, w2i t2)
                     val t2' = sub (sbox, w2i t3)
                     val t3' = sub (sbox, w2i t0)
                  in
                     loop2 (4, pos, i+1, t0', t1', t2', t3')
                  end

            and loop2 (n, pos, i, t0, t1, t2, t3) =
               if n <= 0 then
                  loop1 (pos, i, t0, t1, t2, t3)
               else
                  let
                     val t0' = xorb (t0, asub (fkey, pos-16))
                     val t1' = xorb (t1, asub (fkey, pos-15))
                     val t2' = xorb (t2, asub (fkey, pos-14))
                     val t3' = xorb (t3, asub (fkey, pos-13))

                     val () = A.update (fkey, pos, t0')
                     val () = A.update (fkey, pos+1, t1')
                     val () = A.update (fkey, pos+2, t2')
                     val () = A.update (fkey, pos+3, t3')
                  in
                     loop2 (n-1, pos+4, i, t0', t1', t2', t3')
                  end

            val () =
               loop1 (16, 1, sub (keyv, 12), sub (keyv, 13), sub (keyv, 14), sub (keyv, 15))
         in
            fkey
         end

      fun fullkey192 prekey =
         let
            val keyv = B.toWord8Vector prekey

            val fkey = Word8Array.array (208, 0w0)
            val () = Word8Array.copyVec {src=keyv, dst=fkey, di=0}

            fun loop1 (pos, i, t0, t1, t2, t3) =
               let
                  val t0' = xorb (sub (sbox, w2i t1), sub (rcon, i))
                  val t1' = sub (sbox, w2i t2)
                  val t2' = sub (sbox, w2i t3)
                  val t3' = sub (sbox, w2i t0)
               in
                  loop2 (6, pos, i+1, t0', t1', t2', t3')
               end

            (* 24 doesn't divide 208, so we have to check termination down here. *)

            and loop2 (n, pos, i, t0, t1, t2, t3) =
               if pos >= 208 then
                  ()
               else if n <= 0 then
                  loop1 (pos, i, t0, t1, t2, t3)
               else
                  let
                     val t0' = xorb (t0, asub (fkey, pos-24))
                     val t1' = xorb (t1, asub (fkey, pos-23))
                     val t2' = xorb (t2, asub (fkey, pos-22))
                     val t3' = xorb (t3, asub (fkey, pos-21))

                     val () = A.update (fkey, pos, t0')
                     val () = A.update (fkey, pos+1, t1')
                     val () = A.update (fkey, pos+2, t2')
                     val () = A.update (fkey, pos+3, t3')
                  in
                     loop2 (n-1, pos+4, i, t0', t1', t2', t3')
                  end

            val () =
               loop1 (24, 1, sub (keyv, 20), sub (keyv, 21), sub (keyv, 22), sub (keyv, 23))
         in
            fkey
         end

      fun fullkey256 prekey =
         let
            val keyv = B.toWord8Vector prekey

            val fkey = Word8Array.array (240, 0w0)
            val () = Word8Array.copyVec {src=keyv, dst=fkey, di=0}

            fun loop1 (pos, i, t0, t1, t2, t3) =
               let
                  val t0' = xorb (sub (sbox, w2i t1), sub (rcon, i))
                  val t1' = sub (sbox, w2i t2)
                  val t2' = sub (sbox, w2i t3)
                  val t3' = sub (sbox, w2i t0)
               in
                  loop2 (4, pos, i+1, t0', t1', t2', t3')
               end

            and loop2 (n, pos, i, t0, t1, t2, t3) =
               if pos >= 240 then
                  ()
               else if n <= 0 then
                  loop3 (pos, i, t0, t1, t2, t3)
               else
                  let
                     val t0' = xorb (t0, asub (fkey, pos-32))
                     val t1' = xorb (t1, asub (fkey, pos-31))
                     val t2' = xorb (t2, asub (fkey, pos-30))
                     val t3' = xorb (t3, asub (fkey, pos-29))

                     val () = A.update (fkey, pos, t0')
                     val () = A.update (fkey, pos+1, t1')
                     val () = A.update (fkey, pos+2, t2')
                     val () = A.update (fkey, pos+3, t3')
                  in
                     loop2 (n-1, pos+4, i, t0', t1', t2', t3')
                  end

            and loop3 (pos, i, t0, t1, t2, t3) =
               let
                  val t0' = sub (sbox, w2i t0)
                  val t1' = sub (sbox, w2i t1)
                  val t2' = sub (sbox, w2i t2)
                  val t3' = sub (sbox, w2i t3)
               in
                  loop4 (4, pos, i, t0', t1', t2', t3')
               end

            and loop4 (n, pos, i, t0, t1, t2, t3) =
               if n <= 0 then
                  loop1 (pos, i, t0, t1, t2, t3)
               else
                  let
                     val t0' = xorb (t0, asub (fkey, pos-32))
                     val t1' = xorb (t1, asub (fkey, pos-31))
                     val t2' = xorb (t2, asub (fkey, pos-30))
                     val t3' = xorb (t3, asub (fkey, pos-29))

                     val () = A.update (fkey, pos, t0')
                     val () = A.update (fkey, pos+1, t1')
                     val () = A.update (fkey, pos+2, t2')
                     val () = A.update (fkey, pos+3, t3')
                  in
                     loop4 (n-1, pos+4, i, t0', t1', t2', t3')
                  end

            val () =
               loop1 (32, 1, sub (keyv, 28), sub (keyv, 29), sub (keyv, 30), sub (keyv, 31))
         in
            fkey
         end

      fun makeKey (prekey, ()) =
         (case B.size prekey of
             16 => fullkey128 prekey
           | 24 => fullkey192 prekey
           | 32 => fullkey256 prekey
           | _ => raise Domain)

         
      fun encrypt (fkey, msg) =
         if Bytestring.size msg <> 16 then
            raise Domain
         else
            let
               val count = A.length fkey - 16

               fun loop (i, a00, a10, a20, a30, a01, a11, a21, a31, a02, a12, a22, a32, a03, a13, a23, a33) =
                  if i = count then
                     Bytestring.implode
                     [xorb (sub (sbox, w2i a00), asub (fkey, i)),
                      xorb (sub (sbox, w2i a11), asub (fkey, i+1)),
                      xorb (sub (sbox, w2i a22), asub (fkey, i+2)),
                      xorb (sub (sbox, w2i a33), asub (fkey, i+3)),
                      xorb (sub (sbox, w2i a01), asub (fkey, i+4)),
                      xorb (sub (sbox, w2i a12), asub (fkey, i+5)),
                      xorb (sub (sbox, w2i a23), asub (fkey, i+6)),
                      xorb (sub (sbox, w2i a30), asub (fkey, i+7)),
                      xorb (sub (sbox, w2i a02), asub (fkey, i+8)),
                      xorb (sub (sbox, w2i a13), asub (fkey, i+9)),
                      xorb (sub (sbox, w2i a20), asub (fkey, i+10)),
                      xorb (sub (sbox, w2i a31), asub (fkey, i+11)),
                      xorb (sub (sbox, w2i a03), asub (fkey, i+12)),
                      xorb (sub (sbox, w2i a10), asub (fkey, i+13)),
                      xorb (sub (sbox, w2i a21), asub (fkey, i+14)),
                      xorb (sub (sbox, w2i a32), asub (fkey, i+15))]
                  else
                     let
                        val (b00, b10, b20, b30) =
                           mixColumns
                           (sub (sbox, w2i a00), sub (sbox, w2i a11), sub (sbox, w2i a22), sub (sbox, w2i a33))

                        val (b01, b11, b21, b31) =
                           mixColumns
                           (sub (sbox, w2i a01), sub (sbox, w2i a12), sub (sbox, w2i a23), sub (sbox, w2i a30))

                        val (b02, b12, b22, b32) =
                           mixColumns
                           (sub (sbox, w2i a02), sub (sbox, w2i a13), sub (sbox, w2i a20), sub (sbox, w2i a31))
                           
                        val (b03, b13, b23, b33) =
                           mixColumns
                           (sub (sbox, w2i a03), sub (sbox, w2i a10), sub (sbox, w2i a21), sub (sbox, w2i a32))
                     in
                        loop (i+16,
                              xorb (b00, asub (fkey, i)),
                              xorb (b10, asub (fkey, i+1)),
                              xorb (b20, asub (fkey, i+2)),
                              xorb (b30, asub (fkey, i+3)),
                              xorb (b01, asub (fkey, i+4)),
                              xorb (b11, asub (fkey, i+5)),
                              xorb (b21, asub (fkey, i+6)),
                              xorb (b31, asub (fkey, i+7)),
                              xorb (b02, asub (fkey, i+8)),
                              xorb (b12, asub (fkey, i+9)),
                              xorb (b22, asub (fkey, i+10)),
                              xorb (b32, asub (fkey, i+11)),
                              xorb (b03, asub (fkey, i+12)),
                              xorb (b13, asub (fkey, i+13)),
                              xorb (b23, asub (fkey, i+14)),
                              xorb (b33, asub (fkey, i+15)))
                     end
            in
               loop (16,
                     xorb (bsub (msg, 0), asub (fkey, 0)),
                     xorb (bsub (msg, 1), asub (fkey, 1)),
                     xorb (bsub (msg, 2), asub (fkey, 2)),
                     xorb (bsub (msg, 3), asub (fkey, 3)),
                     xorb (bsub (msg, 4), asub (fkey, 4)),
                     xorb (bsub (msg, 5), asub (fkey, 5)),
                     xorb (bsub (msg, 6), asub (fkey, 6)),
                     xorb (bsub (msg, 7), asub (fkey, 7)),
                     xorb (bsub (msg, 8), asub (fkey, 8)),
                     xorb (bsub (msg, 9), asub (fkey, 9)),
                     xorb (bsub (msg, 10), asub (fkey, 10)),
                     xorb (bsub (msg, 11), asub (fkey, 11)),
                     xorb (bsub (msg, 12), asub (fkey, 12)),
                     xorb (bsub (msg, 13), asub (fkey, 13)),
                     xorb (bsub (msg, 14), asub (fkey, 14)),
                     xorb (bsub (msg, 15), asub (fkey, 15)))
            end


      fun decrypt (fkey, msg) =
         if Bytestring.size msg <> 16 then
            raise Domain
         else
            let
               val count = A.length fkey - 16

               fun loop (i, a00, a10, a20, a30, a01, a11, a21, a31, a02, a12, a22, a32, a03, a13, a23, a33) =
                  if i = 0 then
                     Bytestring.implode
                     [xorb (a00, asub (fkey, 0)),
                      xorb (a10, asub (fkey, 1)),
                      xorb (a20, asub (fkey, 2)),
                      xorb (a30, asub (fkey, 3)),
                      xorb (a01, asub (fkey, 4)),
                      xorb (a11, asub (fkey, 5)),
                      xorb (a21, asub (fkey, 6)),
                      xorb (a31, asub (fkey, 7)),
                      xorb (a02, asub (fkey, 8)),
                      xorb (a12, asub (fkey, 9)),
                      xorb (a22, asub (fkey, 10)),
                      xorb (a32, asub (fkey, 11)),
                      xorb (a03, asub (fkey, 12)),
                      xorb (a13, asub (fkey, 13)),
                      xorb (a23, asub (fkey, 14)),
                      xorb (a33, asub (fkey, 15))]
                  else
                     let
                        val (b00, b10, b20, b30) =
                           invMixColumns
                           (xorb (a00, asub (fkey, i)), xorb (a10, asub (fkey, i+1)), xorb (a20, asub (fkey, i+2)), xorb (a30, asub (fkey, i+3)))

                        val (b01, b11, b21, b31) =
                           invMixColumns
                           (xorb (a01, asub (fkey, i+4)), xorb (a11, asub (fkey, i+5)), xorb (a21, asub (fkey, i+6)), xorb (a31, asub (fkey, i+7)))

                        val (b02, b12, b22, b32) =
                           invMixColumns
                           (xorb (a02, asub (fkey, i+8)), xorb (a12, asub (fkey, i+9)), xorb (a22, asub (fkey, i+10)), xorb (a32, asub (fkey, i+11)))

                        val (b03, b13, b23, b33) =
                           invMixColumns
                           (xorb (a03, asub (fkey, i+12)), xorb (a13, asub (fkey, i+13)), xorb (a23, asub (fkey, i+14)), xorb (a33, asub (fkey, i+15)))

                     in
                        loop (i-16,
                              sub (invSbox, w2i b00),
                              sub (invSbox, w2i b13),
                              sub (invSbox, w2i b22),
                              sub (invSbox, w2i b31),
                              sub (invSbox, w2i b01),
                              sub (invSbox, w2i b10),
                              sub (invSbox, w2i b23),
                              sub (invSbox, w2i b32),
                              sub (invSbox, w2i b02),
                              sub (invSbox, w2i b11),
                              sub (invSbox, w2i b20),
                              sub (invSbox, w2i b33),
                              sub (invSbox, w2i b03),
                              sub (invSbox, w2i b12),
                              sub (invSbox, w2i b21),
                              sub (invSbox, w2i b30))
                     end
            in
               loop (count-16,
                     sub (invSbox, w2i (xorb (bsub (msg, 0), asub (fkey, count)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 13), asub (fkey, count+13)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 10), asub (fkey, count+10)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 7), asub (fkey, count+7)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 4), asub (fkey, count+4)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 1), asub (fkey, count+1)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 14), asub (fkey, count+14)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 11), asub (fkey, count+11)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 8), asub (fkey, count+8)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 5), asub (fkey, count+5)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 2), asub (fkey, count+2)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 15), asub (fkey, count+15)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 12), asub (fkey, count+12)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 9), asub (fkey, count+9)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 6), asub (fkey, count+6)))),
                     sub (invSbox, w2i (xorb (bsub (msg, 3), asub (fkey, count+3)))))
            end

   end


structure AESCBC = CBCCipherFun (structure Cipher = AES)
structure AESCFB = CFBCipherFun (structure Cipher = AES)
structure AESOFB = OFBCipherFun (structure Cipher = AES)
structure AESCTR = CTRCipherFun (structure Cipher = AES)
