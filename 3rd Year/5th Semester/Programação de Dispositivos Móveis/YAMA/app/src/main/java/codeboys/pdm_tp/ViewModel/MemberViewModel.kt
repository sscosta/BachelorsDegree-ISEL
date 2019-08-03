package codeboys.pdm_tp.ViewModel

import android.app.Application
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.LiveData
import codeboys.pdm_tp.MessagingApp
import codeboys.pdm_tp.Model.Member


class MemberViewModel(val app : Application) : AndroidViewModel(app){

    private val membersRepository = (app as MessagingApp).repository.memberRepository

    fun getMembers(idTeam : Int): LiveData<List<Member>> {
        return membersRepository.getMembers(idTeam)
    }

}
