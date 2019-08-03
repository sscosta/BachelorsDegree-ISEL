package codeboys.pdm_tp.ViewModel

import android.app.Application
import androidx.lifecycle.AndroidViewModel
import codeboys.pdm_tp.repository


class TeamViewModel(val app : Application) : AndroidViewModel(app){


    val teams = app.repository.teamRepository.allTeams

    fun loadTeams(){
        app.repository.teamRepository.loadTeams()
    }

}
