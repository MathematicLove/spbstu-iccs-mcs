#pragma once

enum { PAS = 300, NOCARD = 400 };
//PAS - ÔËÁÌ‡Í ÔÓÔÛÒÍ‡ ıÓ‰‡ (ÌÂ˜ÂÏ ıÓ‰ËÚ¸ ËÎË Í˚Ú¸)
//NOCARD - ÔËÁÌ‡Í ÔÓÔÛÒÍ‡ ıÓ‰‡ (ÌÂÚ Í‡Ú Ì‡ ÛÍ‡ı)

static const char *suits[]     = { "Clubs", "Spades", "Hearts", "Diamonds" }; // “ÂÙ˚ œËÍË ◊Â‚˚ ¡Û·Ë
static const char *suitsSymb[] = { "\x5", "\x6", "\x3", "\x4" };
static const char *ranks[] = { "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"};
                            //  0    1    2    3    4     5      6       7
struct Card
{
private:
    int suit;
    int rank;

    bool operator >(const Card &card);
    friend class Dealer;
    
    Card(int suit = -1, int rank = -1);
public:
    
};


class Dealer
{
public:
    static const int maxSuits = 4;
    static const int maxRanks = 13;
    static const int maxTrick = 6;

private:
    static int currentCard;
    static Card *trump;  // Í‡Ú‡ - ÍÓÁ˚¸
    static Card *noCard, *pasCard; // Í‡Ú‡ - ÔËÁÌ‡Í "ÌÂÚ Í‡Ú" Ë "Ô‡Ò"
    static Card deck[maxSuits*maxRanks]; //ÍÓÎÓ‰‡

    static bool tableRanks[maxRanks];   // ‡Ì„Ë Í‡Ú, ÔËÒÛÚÒÚ‚Û˛˘Ëı Ì‡ ÒÚÓÎÂ
    
    static int currentHeadTrik; //ÌÓÏÂ ıÓ‰‡ ‚ ÍÓÌÛ

    static Card *headTrick[2][maxTrick]; // ÒÚÓÎ [0] - ıÓ‰ Ë„ÓÍ‡, [1] - ÓÚ·ÓÈ Í‡Ú˚

    static void GenerateDeck();
    


public:

    //ÔÂÂÏÂ¯‡Ú¸ ÍÓÎÓ‰Û - ËÌËˆË‡ÎËÁËÛÂÚ ÍÓÎÓ‰Û Ë ‚ÒÂ ÔÂÂÏÂÌÌ˚Â.
    //‚˚·Ë‡ÂÚ ÍÓÁ˚ˇ
    static void ShuffleDec();

    //‚ÁˇÚ¸ Í‡ÚÛ ËÁ ÍÓÎÓ‰˚. ‚ÓÁ‚‡˘‡ÂÚ true, ÂÒÎË Í‡Ú˚ Â˘Â ÓÒÚ‡ÎËÒ¸.
    static bool GetCard(Card * &outCard);
    
    //‚ÓÁ‚‡˘‡ÂÚ ÚÂÍÛ˘Â„Ó ÍÓÁ˚ˇ ‚ ÒÚÛÍÚÛÂ Card (ËÏÂÂÚ ÁÌ‡˜ÂÌËÂ ÚÓÎ¸ÍÓ Ï‡ÒÚ¸).
    static Card *GetTrump();
    
    // ‚ÓÁ‚‡˘‡ÂÚ ˜ËÒÎÓ ‚˚¯Â‰¯Ëı ËÁ ÍÓÎÓ‰˚ Í‡Ú
    static int getcurrentCard();

    //‚ÓÁ‚‡˘‡ÂÚ ÛÍ‡Á‡ÚÂÎ¸ Ì‡ ÒÚÓÎ
    static Card * (*GetheadTrick())[maxTrick];

    //‚ÓÁ‚‡˘‡ÂÚ ÒÚÓÍÓ‚˚È ÎËÚÂ‡Î ÒÓ‰ÂÊ‡˘ËÈ Ï‡ÒÚ¸ ËÎË ‰ÓÒÚÓËÌÒÚ‚Ó Í‡Ú˚
    static const char * SuitName(const Card *card);
    static const char * RankName(const Card *card);

    // * ‚ÓÁ‚‡˘‡ÂÚ ËÌ‰ÂÍÒ ÒÓÓÚ‚ÂÚÒÚ‚Û˛˘ËÈ Ï‡ÒÚË ËÎË ‰ÓÒÚÓËÌÒÚ‚Û Í‡Ú˚
    static int SuitIndex(const Card *card);
    static int RankIndex(const Card *card);

    // ‚ÓÁ‚‡˘‡ÂÚ ÌÓÏÂ ıÓ‰‡ (0-5)
    static int GetCurrentHeadTrik();
    
    // œÓ‚ÂˇÂÚ ‚ÓÁÏÓÊÂÌ ÎË ÒÎÂ‰Û˛˘ËÈ ıÓ‰ (ıÓ‰Ó‚<6, ÓÚ·Ë‚‡˛˘ËÈÒˇ Ë„ÓÍ ÌÂ ÒÔ‡ÒÓ‚‡Î)
    static bool NextTrikEnable();


    // * ‚˚‚Ó‰ËÚ Ì‡ ˝Í‡Ì Í‡ÚÛ ËÎË ÒÚÓÎ
    static void ShowCard(const Card *card);
    static void ShowTable();
    
    //* ‚ÓÁ‚‡˘‡ÂÚ ÛÍ‡Á‡ÚÂÎ¸ Ì‡ Í‡ÚÛ "Ô‡Ò" ËÎË "ÌÂÚ Í‡Ú˚"
    static Card *GetPas();
    static Card *GetNocard();
    
    // ‚ÓÁ‚‡˘‡ÂÚ ÛÍ‡Á‡ÚÂÎ¸ Ì‡ ÔÓÒÎÂ‰Ì˛˛ Í‡ÚÛ Ò ÍÓÚÓÓÈ ıÓ‰ËÎË Ë ÍÓÚÓÓÈ ÓÚ·Ë‚‡ÎËÒ¸
    static Card *GetLastCard();
    static Card *GetLastDefendCard();
    
    //Ó˜Ë˘‡ÂÚ ÒÚÓÎ
    static void ClearTable();

    //ÔÓ‚ÂˇÂÚ Í‡Ú˚ Ì‡ ÒÚÓÎÂ Ì‡ ÍÓÂÍÚÌÓÒÚ¸
    static bool CheckHeadTrick();

    //‡Ú‡ÍÓ‚‡Ú¸ Ë„ÓÍ‡ Í‡ÚÓÈ card
    static void Attack(Card *card);
    
    //ÔÓÍ˚Ú¸ ‡Ú‡ÍÛ˛˘Û˛ Í‡ÚÛ, Í‡ÚÓÈ card
    static void Defend(Card *card);

    ~Dealer();
};

