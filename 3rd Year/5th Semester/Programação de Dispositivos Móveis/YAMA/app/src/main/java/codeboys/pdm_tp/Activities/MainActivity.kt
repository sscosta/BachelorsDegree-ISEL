package codeboys.pdm_tp.Activities

import android.content.Intent
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import codeboys.pdm_tp.Fragments.ProfileFragment
import codeboys.pdm_tp.Fragments.TeamFragment
import codeboys.pdm_tp.PageAdapter
import codeboys.pdm_tp.R
import codeboys.pdm_tp.repository
import kotlinx.android.synthetic.main.activity_main.*

class MainActivity : AppCompatActivity() {

    var pagerAdapter: PageAdapter?=null



    override fun onCreate(savedInstanceState: Bundle?) {

        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        if(application.repository.getProfileSharedData(this)==null)
            startActivity(Intent(this, LoginActivity::class.java))

        pagerAdapter = PageAdapter(supportFragmentManager)
        pagerAdapter!!.addFragments(TeamFragment(),"Teams")
        pagerAdapter!!.addFragments(ProfileFragment(),"Profile")

        //adding pageadapter to viewpage
        mainViewPage.adapter = pagerAdapter
        //Now setting up viewpager with tablayout
        mainTabPage.setupWithViewPager(mainViewPage)

    }
}
