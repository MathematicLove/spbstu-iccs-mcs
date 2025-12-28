#include "dealer.h"
#include "player0307.h"
#include "player0307.h"
//#include <vld.h>

#include <iostream>



int main()
{
    Dealer deal;
    int p1win = 0, p2win = 0, drawn =0;
    for (int count = 0; count < 100; count++)
    {
         Dealer::ShuffleDec();

        Card *temp;
        Player0307 p1("Nickola");
        Player0307 p2("Kirill");
        //Player0176 p2("Grigory   ");


        p1.ShowCards();

        PlayerAbstract *player1;
        PlayerAbstract *player2;
        PlayerAbstract *tmp;

        if (count % 2)
        {
            p1.YouTurn(true);
            p2.YouTurn(false);
            player1 = &p1;
            player2 = &p2;

        }
        else
        {
            p1.YouTurn(false);
            p2.YouTurn(true);
            player1 = &p2;
            player2 = &p1;

        }

        //    Player *player = p2;

        while (player1->INeedCard())
        {
            Dealer::GetCard(temp);
            player1->TakeOneCard(temp);
        }
        while (player2->INeedCard())
        {
            Dealer::GetCard(temp);
            player2->TakeOneCard(temp);
        }

        int triknum = 1;

        while (
            (player1->GetCardNum() && player2->GetCardNum()) || Dealer::getcurrentCard() < 52  //œÓÍ‡ ÂÒÚ¸ Í‡Ú˚ Ì‡ ÛÍ‡ı Û Ó·ÓËı Ë„ÓÍÓ‚  ËÎË ÂÒÚ¸ Í‡Ú˚ ‚ ÍÓÎÓ‰Â
            )
        {
            std::cout << triknum++ << ":" << std::endl;
            while (player1->INeedCard())
            {
                Dealer::GetCard(temp);
                player1->TakeOneCard(temp);
            }
            while (player2->INeedCard())
            {
                Dealer::GetCard(temp);
                player2->TakeOneCard(temp);
            }

            player1->ShowCards();
            player2->ShowCards();
            while (Dealer::NextTrikEnable())
            {
                // ¬˚‚Ó‰ ÍÓÁ˚ÌÓÈ Í‡Ú˚ Ì‡ Ô‡ÚË˛
                std::cout << "Trump :" << std::endl;
                std::cout << ranks[Dealer::RankIndex(Dealer::GetTrump())] << suitsSymb[Dealer::SuitIndex(Dealer::GetTrump())] << "\n " << std::endl;
                player1->ShowCards();
                player2->ShowCards();

                player1->PutCard();
                Dealer::ShowTable();
                player2->GetHeadTrick();
                Dealer::ShowTable();
            }
            Dealer::ShowTable();
            int kht = Dealer::GetCurrentHeadTrik();
            // ÔÓ‚ÂËÚ¸ ‚Ò∏ ÔÓ Ô‡‡Ï
            if (Dealer::CheckHeadTrick()) // Í‡Ú˚ ÓÚ·ËÚ˚ ‚ÂÌÓ
            {
                Card * plastDefCard = Dealer::GetLastDefendCard();
                Card * plastCard = Dealer::GetLastCard();
                if (plastDefCard != nullptr)
                {
                    // ÂÒÎË ÔÓÒÎÂ‰Ìˇ Í‡Ú‡ ÓÚ·Óˇ  "Ô‡Ò" - ‚ÚÓÓÈ Á‡·Ë‡ÂÚ Í‡Ú˚ ÒÂ·Â
                    if (Dealer::RankIndex(plastDefCard) == PAS)
                    {
                        player2->TakeCards();
                        Dealer::ClearTable();
                    }
                    // ÂÒÎË ÔÓÒÎÂ‰Ìˇˇ Í‡Ú‡ Ò ÍÓÚÓÓÈ ıÓ‰ËÎË - Ô‡Ò ËÎË ÌÂÚ Í‡Ú - ÔÂÂıÓ‰ ıÓ‰‡, ÓÚ·ÓÈ
                    else if ((Dealer::RankIndex(plastCard) == PAS) ||
                        (Dealer::RankIndex(plastCard) == NOCARD) ||
                        (Dealer::GetCurrentHeadTrik() == 6)
                        )
                    {
                        Dealer::ClearTable();
                        player2->YouTurn(true);
                        player1->YouTurn(false);
                        tmp = player2;
                        player2 = player1;
                        player1 = tmp;
                    }
                }
            }
            else
            {
                std::cout << "error";
            }
        }

        if (!player1->GetCardNum() && !player2->GetCardNum())
        {
            drawn++;
            std::cout << "\ndrawn game\n";
        }
        else if (!player1->GetCardNum())
        {
            std::cout << "\np1 - win\n";
            if (player1 == &p1)
                p1win++;
            else
                p2win++;
        }
            
        else if (!player2->GetCardNum())
        {
            std::cout << "\np2 - win\n";
            if (player2 == &p2)
                p2win++;
            else
                p1win++;
        }
            
        // ÂÒÎË ÔÓÒÎÂ‰Ìˇˇ Í‡Ú‡ - Ô‡Ò - ‚ÚÓÓÈ Á‡·Ë‡ÂÚ

        player1->ShowCards();
        player2->ShowCards();

        std::cout << "\nNickola  - " << p1win << "\nKirill - " << p2win << "\ndrawn " << drawn << "\n";
    }
    
    return 0;
}

