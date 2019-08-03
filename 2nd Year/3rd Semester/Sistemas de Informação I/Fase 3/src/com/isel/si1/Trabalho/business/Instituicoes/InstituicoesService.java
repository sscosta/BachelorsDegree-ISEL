package com.isel.si1.Trabalho.business.Instituicoes;

import com.isel.si1.Trabalho.data.Instituicoes.IInstituicoesDAO;
import com.isel.si1.Trabalho.data.Instituicoes.InstituicoesDAOFactory;
import com.isel.si1.Trabalho.data.SQLServerConnectionFactory;
import com.isel.si1.Trabalho.model.Instituicoes;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class InstituicoesService implements IIntituicoesService {


    private static Logger logger = Logger.getLogger(InstituicoesService.class.getName());

    @Override
    public List<Instituicoes> getAllInstituicoes() {

        try {
            Connection       aConnection = SQLServerConnectionFactory.getInstance().getConnection();
            IInstituicoesDAO anInstituicoesDAO = InstituicoesDAOFactory.getNewInstance(aConnection);

            return anInstituicoesDAO.getAllInstituicoes();

//		System.out.println("Sleeping...");
//		Thread.sleep(10000);
        } catch (ClassNotFoundException | SQLException theCause) {
            logger.log(Level.SEVERE, "an exception was thrown", theCause);
        }

        return Collections.emptyList();
    }
}
