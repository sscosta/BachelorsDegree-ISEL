package codeboys.pdm_tp.Fragments

import android.content.Intent
import android.os.Bundle
import android.util.Log
import androidx.fragment.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.AdapterView
import android.widget.ListAdapter
import android.widget.SimpleAdapter
import androidx.lifecycle.Observer
import androidx.lifecycle.ViewModelProviders
import codeboys.pdm_tp.Activities.MemberActivity
import codeboys.pdm_tp.Model.Team
import codeboys.pdm_tp.R
import codeboys.pdm_tp.ViewModel.TeamViewModel
import kotlinx.android.synthetic.main.fragment_team.*

class TeamFragment : Fragment() {

    private val teamsModel by lazy {
        ViewModelProviders
                .of(this)
                .get(TeamViewModel::class.java)
    }

    override fun onCreateView(
                                inflater: LayoutInflater,
                                container: ViewGroup?,
                                savedInstanceState: Bundle?
                            ): View? {

        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_team, container, false)
    }

    override fun onResume() {
        super.onResume()
        Log.d("Team Fragment","onResume")
        teamsModel.teams.observe(this, Observer<List<Team>> {
            teamsListView.adapter = buildAdapter(it)

            teamsListView.onItemClickListener =
                    AdapterView.OnItemClickListener { parent, view, position, id ->
                        val intent = Intent(activity, MemberActivity::class.java)
                        intent.putExtra("idTeam", (teamsListView.adapter.getItem(position) as Map<String, Int>)["id"])
                        startActivity(intent)
                    }
        }
        )
        teamsModel.loadTeams()
    }

    private fun buildAdapter(teams: List<Team>): ListAdapter = SimpleAdapter(
                    activity,
                    teams.map {
                        team -> mapOf(
                            "name" to team.name,
                            "id" to team.id
                        )
                    },
                    android.R.layout.simple_list_item_2,
                    arrayOf("name","id"),
                    intArrayOf(android.R.id.text1,android.R.id.text2)
    )

}
