#pragma once
#include "dealer.h"

class PlayerAbstract
{
protected:
    

public:
    virtual ~PlayerAbstract() {};

    //ÔÓÎÛ˜‡ÂÚ ÔËÁÌ‡Í "ÏÓÈ ıÓ‰"
    virtual void YouTurn(bool) = 0;

    //Ë„ÓÍ ÍÎ‡‰∏Ú Í‡ÚÛ Ì‡ ÒÚÓÎ (headTrick[0][*])
    virtual void PutCard() = 0;

    // Á‡·Ë‡ÂÚ ‚ÒÂ Í‡Ú˚ ÒÓ ÒÚÓÎ‡
    virtual void TakeCards() = 0;

    // ÓÚ·Ë‚‡ÂÚ Í‡ÚÛ (ÍÎ‡‰∏Ú Í‡ÚÛ ‚ (headTrick[1][*])
    virtual void GetHeadTrick() = 0;

    //‚ÁˇÎ Ó‰ÌÛ Í‡ÚÛ
    virtual void TakeOneCard(Card * &nc) = 0;

    // ‚˚‚ÂÎ Ò‚ÓË Í‡Ú˚ Ì‡ ˝Í‡Ì ‡Ì„ (Ó‰ËÌ/‰‚‡ ÒËÏ‚ÓÎ‡, Ï‡ÒÚ¸ - ÒËÏ‚ÓÎ)
    virtual void ShowCards() = 0;

    // ‚ÓÁ‚‡˘‡ÂÚ ËÒÚËÌÛ, ÂÒÎË Ì‡ ÛÍ‡ı Í‡Ú ÏÂÌ¸¯Â 6
    virtual bool INeedCard() = 0;

    // ‚ÓÁ‚‡˘‡ÂÚ ˜ËÒÎÓ Í‡Ú Ì‡ ÛÍ‡ı
    virtual int GetCardNum() = 0;
};

class Player0307 : public PlayerAbstract
{
    const char* NAME;
    bool Turn;
    unsigned int count;
    Card* Hand[52]; // K K K K K K 0 0 0 ... -->ÔÓıÓ‰ËÎ 0 K K K K K 0 0 0...-->‚ÁˇÎ  K K K K K K 0 0 0...

// –Â‡ÎËÁÛÈÚÂ ËÌÚÂÙÂÈÒ˚ ‡·ÒÚ‡ÍÚÌÓ„Ó ÍÎ‡ÒÒ‡
// ƒÓÒÚÛÔÌ˚Â ÏÂÚÓ‰˚ ËÁ ÍÎ‡ÒÒ‡ Dealer ÏÓÊÌÓ Û‚Ë‰ÂÚ¸ ‚ Ù‡ÈÎÂ dealer.h

public:
    //ÍÓÌÒÚÛÍÚÓ
    Player0307(const char* name) {
        NAME = name;
        Turn = 0;
        count = 0;
        for (int i = 0; i < 52; i++){
            Hand[i] = nullptr;
        }
    }
    
    void YouTurn(bool flag);

    void PutCard();
    void TakeCards();
    void GetHeadTrick();

    void TakeOneCard(Card*& nc);
    void DeleteOneCard(Card* nc);
    void ShowCards();

    bool INeedCard();
    int GetCardNum();

    Card* SearchMinNonTrump();
    Card* SearchMinTrump();

    Card* SearchRankInHand(Card* pudge);
    bool AllTrumpsHand();
};


