#include "player0307.h"
#include "dealer.h"
#include <iostream>

void Player0307::YouTurn(bool flag) {
    Turn = flag;
}

void Player0307::PutCard() {   // ÂÒÎË ıÓ‰ ÔÂ‚˚È ‚ ÍÓÌÛ, ÚÓ Ó‰ÌË Ô‡‚ËÎ‡ ‰Îˇ ‚˚·Ó‡ Í‡Ú˚, ÂÒÎË ÌÂ ÔÂ‚˚È, ÚÓ ‰Û„ËÂ
    Card* NoCard = Dealer::GetNocard();
    Card* Pas = Dealer::GetPas();
    Card* LastCard = Dealer::GetLastCard();
    Card* LastDefCard = Dealer::GetLastDefendCard();

    Card* MinNonTrump = this->SearchMinNonTrump();
    Card* MinTrump = this->SearchMinTrump();
    int NowTrump = Dealer::SuitIndex(Dealer::GetTrump());// Á‡ÔËÒ˚‚‡ÂÏ Ï‡ÒÚ¸ ÍÓÁ˚ÌÓÈ Ï‡ÒÚË

    if (Dealer::GetCurrentHeadTrik() == 0) { // ıÓ‰ ‚ ÍÓÌÛ ÔÂ‚˚È , ÒÚÓÎ ÔÛÒÚ

        if (count == 0) {
            Dealer::Attack(NoCard);
            return;
        }
        else if (MinNonTrump != nullptr) {
            Dealer::Attack(MinNonTrump);
            this->DeleteOneCard(MinNonTrump);
            return;
        }
        else if (MinTrump != nullptr) {
            Dealer::Attack(MinTrump);
            this->DeleteOneCard(MinTrump);
            return;
        }
    }
    else {  // ıÓ‰ ‚ ÍÓÌÛ ÌÂ ÔÂ‚˚È, Ì‡ ÒÚÓÎÂ ˜ÚÓ-ÚÓ ÂÒÚ¸

        int LastPlaySuit = Dealer::SuitIndex(LastCard); // ‚˚˜ËÒÎÂÌËÂ Ï‡ÒÚË
        int LastPlayRank = Dealer::RankIndex(LastCard); // Ë ‰ÓÒÚÓËÌÒÚ‚‡ Í‡Ú˚

        int LastDefSuit = Dealer::SuitIndex(LastDefCard); // ‚˚˜ËÒÎÂÌËÂ Ï‡ÒÚË
        int LastDefRank = Dealer::RankIndex(LastDefCard); // Ë ‰ÓÒÚÓËÌÒÚ‚‡ Í‡Ú˚

        if (count == 0) {
            Dealer::Attack(NoCard);
            return;
        }
        else if ( Dealer::SuitIndex(LastDefCard) == NowTrump && Dealer::getcurrentCard() >= 50 )
            // ÂÒÎË ÔÓÚË‚ÌËÍ ÓÚ·ËÎÒˇ ÍÓÁ˚ÌÓÈ Í‡ÚÓÈ Ë ‚ ÍÓÎÓ‰Â ÔÓ˜ÚË ÌÂÚ Í‡Ú,
            //ÓÚ‰‡Ú¸ ÔÂ‰ÔÓ˜ÚÂÌËÂ ÔÓ‰ÍËÌÛÚ¸ Í‡ÚÛ ÚÓÈ ÊÂ Ï‡ÒÚË, ÍÓÚÓÓÈ ıÓ‰ËÎË ‰Ó ˝ÚÓ„Ó
            if (
                this->SearchRankInHand(LastDefCard) != 0 && Dealer::SuitIndex(this->SearchRankInHand(LastDefCard)) == LastPlaySuit
                )
            {
                Dealer::Attack(this->SearchRankInHand(LastDefCard));
                this->DeleteOneCard(this->SearchRankInHand(LastDefCard));
                return;
            }
        else if (this->SearchRankInHand(LastCard) != 0 && Dealer::getcurrentCard()== 52 ) {
            Dealer::Attack(this->SearchRankInHand(LastCard));
            this->DeleteOneCard(this->SearchRankInHand(LastCard));
            return;
        }
        else if (this->SearchRankInHand(LastDefCard) != 0 && Dealer::getcurrentCard() == 52 ) {
            Dealer::Attack(this->SearchRankInHand(LastDefCard));
            this->DeleteOneCard(this->SearchRankInHand(LastDefCard));
            return;
        }
        else if (this->SearchRankInHand(LastCard) != 0 && (Dealer::SuitIndex(this->SearchRankInHand(LastCard))!= NowTrump || this->AllTrumpsHand() == true ) ) {
            Dealer::Attack(   this->SearchRankInHand(LastCard)   );
            this->DeleteOneCard(this->SearchRankInHand(LastCard));
            return;
        }
        else if (this->SearchRankInHand(LastDefCard) != 0 && (Dealer::SuitIndex(this->SearchRankInHand(LastDefCard)) != NowTrump || this->AllTrumpsHand() == true) ) {
            Dealer::Attack(   this->SearchRankInHand(LastDefCard) );
            this->DeleteOneCard(this->SearchRankInHand(LastDefCard));
            return;
        }

        Dealer::Attack(Pas);
        return;
    }
};

void Player0307::TakeCards() {
    int CurHeadTrik = Dealer::GetCurrentHeadTrik(); // —ÍÓÎ¸ÍÓ ıÓ‰Ó‚ ·˚ÎÓ ÒÓ‚Â¯ÂÌÓ ‚ ÍÓÌÂ

    for (int i = 0; i < CurHeadTrik ; i++) {// ≈ÒÎË Í‡Ú‡ ÌÂ Í‡Ú‡-ÔËÁÌ‡Í, ·ÂÂÏ ÂÂ ‚ ÛÍÛ

        if (Dealer::GetheadTrick()[0][i] != Dealer::GetPas()
            && Dealer::GetheadTrick()[0][i] != Dealer::GetNocard() ) {
            this->TakeOneCard(Dealer::GetheadTrick()[0][i]);
        }

        if (Dealer::GetheadTrick()[1][i] != Dealer::GetPas()
            && Dealer::GetheadTrick()[1][i] != Dealer::GetNocard() ) {
            this->TakeOneCard(Dealer::GetheadTrick()[1][i]);
        }
    }
};

void Player0307::GetHeadTrick() {
    Card* NoCard = Dealer::GetNocard();
    Card* Pas = Dealer::GetPas();
    Card* LastCard = Dealer::GetLastCard();
    Card* LastDefCard = Dealer::GetLastDefendCard();

    if (count == 0) {
        Dealer::Defend(NoCard);
        return;
    }
    if (LastCard == Pas) {            // ÂÒÎË ‡Ú‡ÍÛ˛˘ËÈ ËÒÔÓÎ¸ÁÓ‚‡Î Í‡ÚÛ PAS
        Dealer::Defend(NoCard);          // Ó·ÓÓÌˇ˛˘ËÈÒˇ ÍÎ‡‰ÂÚ NOCARD ‚ ÒÓÓÚ‚ÂÚÒÚ‚ËË Ò Ô‡‚ËÎ‡ÏË
        return;
    }
    if (LastCard == NoCard) {         // ÂÒÎË Û ‡Ú‡ÍÛ˛˘Â„Ó Ë„ÓÍ‡ ÌÂÚ Í‡Ú
        if (count == 0) {
            Dealer::Defend(NoCard);
            return;
        }
        else { // count != 0
            Dealer::Defend(Pas);
            return;
        }
    }

    int NowTrump = Dealer::SuitIndex(Dealer::GetTrump()); //‚˚˜ËÒÎÂÌËÂ ÍÓÁ˚ÌÓÈ Ï‡ÒÚË
    int LastPlaySuit = 100;
    int LastPlayRank = 100;
    if (LastCard != 0) {
        LastPlaySuit = Dealer::SuitIndex(LastCard); // ‚˚˜ËÒÎÂÌËÂ Ï‡ÒÚË
        LastPlayRank = Dealer::RankIndex(LastCard); // Ë ‰ÓÒÚÓËÌÒÚ‚‡ Í‡Ú˚, ÍÓÚÓÛ˛ ÌÛÊÌÓ ÓÚ·ËÚ¸
    }
    Card* MinTrumpHand = this->SearchMinTrump(); // ÃËÌËÏ‡Î¸Ì˚È ÍÓÁ˚¸ ‚ ÛÍÂ

    for (int i = 0; i < count; i++) {
        if (Hand[i] != nullptr) {    //œÓËÒÍ Í‡Ú˚, ÍÓÚÓÓÈ ÏÓÊÌÓ ÓÚ·ËÚ¸Òˇ ÔÓ Ô‡‚ËÎ‡Ï
            if (Dealer::RankIndex(Hand[i]) > LastPlayRank && /*(Dealer::RankIndex(Hand[i])-LastPlayRank)< 7 &&*/ Dealer::SuitIndex(Hand[i]) == LastPlaySuit) {
                Dealer::Defend(Hand[i]);
                this->DeleteOneCard(Hand[i]);
                return;
            }
        }
    }
    //¬ ÒÎÛ˜‡Â, ÂÒÎË ‚ ÛÍÂ ÌÂÚ Í‡Ú˚ ÌÛÊÌÓÈ Ï‡ÒÚË, ÚÓ ÂÒÚ¸ ‚ÓÁÏÓÊÌÓÒÚ¸ ÔÓÍ˚Ú¸ ÍÓÁ˚ÂÏ
    if (LastPlaySuit != NowTrump) {
        if ( MinTrumpHand != nullptr &&
            ( (Dealer::RankIndex(MinTrumpHand) >=0  && Dealer::RankIndex(MinTrumpHand) <= 3) || (LastPlayRank <= 6 && Dealer::getcurrentCard() >= 50) || Dealer::getcurrentCard() >=50) ) {
            Dealer::Defend(MinTrumpHand); // ≈ÒÎË ‰ÓÒÚÓËÌÒÚ‚Ó ÏËÌËÏ‡Î¸ÌÓÈ ÍÓÁ˚ÌÓÈ Í‡Ú˚ ‚ ÛÍÂ 2 - 5 ËÎË ÓÚ·Ë‚‡ÂÏÒˇ ÓÚ <=8 ‚ ÍÓÌˆÂ Ë„˚
            this->DeleteOneCard(MinTrumpHand);             // ÚÓ ÏÓÊÌÓ ÍÓÁ˚ÂÏ ÓÚ·ËÚ¸Òˇ ÓÚ ÌÂ ÍÓÁ˚ÌÓÈ Í‡Ú˚
            return;
        }
    }

    //≈ÒÎË ÌË Ó‰ËÌ ËÁ ÒÔÓÒÓ·Ó‚ ÌÂ Ò‡·ÓÚ‡Î, ÚÓ ÁÌ‡˜ËÚ ÓÚ·ËÚ¸Òˇ ÌÂ˜ÂÏ. ¬ Ú‡ÍÓÏ ÒÎÛ˜‡Â ÔÓ Ô‡‚ËÎ‡Ï
    Dealer::Defend(Pas);
    return;
};

void Player0307::TakeOneCard(Card*& nc) {
    for (int i = 0; i < 52; i++) {
        if (Hand[i] == 0) {
            Hand[i] = nc;
            count++;
            break;
        }
    }
}

void Player0307::DeleteOneCard(Card* nc) {
    for (int i = 0; i < 52; i++) {
        if (Hand[i] == nc) {
            Hand[i] = nullptr;
            count--;
            break;
        }
    }
}

void Player0307::ShowCards() {
    for (int i = 0; i < 52; i++){
        if (Hand[i] != nullptr)    {
            std::cout << ranks[Dealer::RankIndex(Hand[i])] << suitsSymb[Dealer::SuitIndex(Hand[i])] << " ";
        }
    }
    std::cout << std::endl;
};






bool Player0307::INeedCard() {
    if (Dealer::getcurrentCard() == 52)
        return false;
    else // ÔÓ ÒÛÚË ÌÂ ÌÛÊÌÓ
        return count < 6;
}

int Player0307::GetCardNum() {
    return count;
}

Card* Player0307::SearchMinNonTrump() {
    Card* MinNonTrump = nullptr;
    int MinRank = 20;
    int NowTrump = Dealer::SuitIndex(Dealer::GetTrump());// Á‡ÔËÒ˚‚‡ÂÏ ËÌ‰ÂÍÒ ÍÓÁ˚ÌÓÈ Ï‡ÒÚË

    for (int i = 0; i < 52; i++) {
        if ((Hand[i] != nullptr) && (Dealer::SuitIndex(Hand[i]) != NowTrump) && (Dealer::RankIndex(Hand[i]) < MinRank)) {
            MinRank = Dealer::RankIndex(Hand[i]);
            MinNonTrump = Hand[i];
        }
    }
    return MinNonTrump;
}

Card* Player0307::SearchMinTrump() {
    Card* MinTrump = nullptr;
    int MinRank = 20;
    int NowTrump = Dealer::SuitIndex(Dealer::GetTrump());// Á‡ÔËÒ˚‚‡ÂÏ ËÌ‰ÂÍÒ ÍÓÁ˚ÌÓÈ Ï‡ÒÚË

    for (int i = 0; i < 52; i++) {
        if (Hand[i] != nullptr && Dealer::SuitIndex(Hand[i]) == NowTrump && Dealer::RankIndex(Hand[i]) < MinRank) {
            MinRank = Dealer::RankIndex(Hand[i]);
            MinTrump = Hand[i];
        }
    }
    return MinTrump;
}
//ÃÂÚÓ‰ ÔÓËÒÍ‡ ‚ ÛÍÂ Í‡Ú˚ ÚÓ„Ó ÊÂ ‰ÓÒÚÓËÌÒÚ‚‡ , ˜ÚÓ Ë Í‡Ú‡-Ô‡‡ÏÂÚ
Card* Player0307::SearchRankInHand(Card* pudge) {
    if (pudge == nullptr) // Á‡˘ËÚ‡ ÓÚ ÌÂÍÓÂÍÚÌÓ„Ó Ô‡‡ÏÂÚ‡
        return nullptr;
                                                //«‡ÔËÒ¸:
    int PudgeRank = Dealer::RankIndex(pudge);//‰ÓÒÚÓËÌÒÚ‚‡ Í‡Ú˚-Ô‡‡ÏÂÚ‡
    int PudgeSuit = Dealer::SuitIndex(pudge);//Ï‡ÒÚË Í‡Ú˚-Ô‡‡ÏÂÚ‡
    int NowTrump = Dealer::SuitIndex(Dealer::GetTrump());//ÍÓÁ˚ÌÓÈ Ï‡ÒÚË

    if (PudgeSuit == NowTrump) {
    // ≈ÒÎË Í‡Ú‡ Ì‡ ÒÚÓÎÂ ÍÓÁ˚ÌÓÈ Ï‡ÒÚË, Ï˚ ÏÓÊÂÏ ÔÓ‰ÍËÌÛÚ¸ Î˛·Û˛ Í‡ÚÛ ÚÓ„Ó ÊÂ ‡Ì„‡
        for (int i = 0; i < 52; i++) {
            if (Hand[i] != 0) {
                if (Dealer::RankIndex(Hand[i]) == PudgeRank)
                    return Hand[i];
            }
        }
    }

    if (PudgeSuit != NowTrump ) {
        // ≈ÒÎË Í‡Ú‡ Ì‡ ÒÚÓÎÂ ÌÂ ÍÓÁ˚Ì‡ˇ Ï˚ ÏÓ˝ÂÏ ÍËÌÛÚ¸ ÍÓÁ˚¸ Ë ÌÂ ÍÓÁ˚¸
        for (int i = 0; i < 52; i++) {
            if (Hand[i] != 0) {
                //ÍÓÁ˚¸ ‚ ÔËÓËÚÂÚÂ ÂÒÎË ËÁ ÍÓÎÓ‰˚ ‚˚¯ÎÓ ÏÌÓ„Ó Í‡Ú
                if (Dealer::RankIndex(Hand[i]) == PudgeRank
                    && Dealer::SuitIndex(Hand[i]) == NowTrump && Dealer::getcurrentCard() > 44)
                    return Hand[i];
                //‚ ÓÒÚ‡Î¸Ì˚ı ÒÎÛ˜‡ˇı ‚˚·Ë‡ÂÏ ÌÂ ÍÓÁ˚ÌÛ˛ Í‡ÚÛ
                else if (Dealer::RankIndex(Hand[i]) == PudgeRank
                    && Dealer::SuitIndex(Hand[i]) != NowTrump)
                    return Hand[i];
            }
        }
    }

    return nullptr;
};

//Card* Player0307::SearchRankNoTrumpInHand(Card* pudge) { //ÃÂÚÓ‰ ÔÓËÒÍ‡ ‚ ÛÍÂ ÌÂÍÓÁ˚ÌÓÈ Í‡Ú˚ ÚÓ„Ó ÊÂ ‰ÓÒÚÓËÌÒÚ‚‡ , ˜ÚÓ Ë Í‡Ú‡-Ô‡‡ÏÂÚ
//    if (pudge == nullptr)
//        return nullptr;
//    int PudgeRank = Dealer::RankIndex(pudge);
//    int NowTrump = Dealer::SuitIndex(Dealer::GetTrump());
//
//    for (int i = 0; i < count; i++) {
//        if (Hand[i] != 0 && Dealer::RankIndex(Hand[i]) == PudgeRank && Dealer::SuitIndex(Hand[i]) != NowTrump ) {
//            if (Dealer::RankIndex(Hand[i]) == PudgeRank)
//                return Hand[i];
//        }
//    }
//
//    return nullptr;
//};

bool Player0307::AllTrumpsHand() {
    int CountTrump = 0;
    int NowTrump = Dealer::SuitIndex(Dealer::GetTrump());// Á‡ÔËÒ˚‚‡ÂÏ ËÌ‰ÂÍÒ ÍÓÁ˚ÌÓÈ Ï‡ÒÚË

    for (int i = 0; i < 52; i++) {
        if (Hand[i] != nullptr && Dealer::SuitIndex(Hand[i]) == NowTrump) {
            CountTrump++;
        }
    }
    if (count - 1 <= CountTrump)
        return true;
    else
        return false;
}









