package project.FitAndFunGym.service;

import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import project.FitAndFunGym.entity.User;
import project.FitAndFunGym.repository.UserRepository;
import project.FitAndFunGym.validator.UserValidator;

import java.util.List;

@Service
public class MyUserDetailService implements UserDetailsService {


    final private UserRepository userRepository;

    final private UserValidator userValidator;

    public MyUserDetailService(UserValidator userValidator, UserRepository userRepository) {
        this.userValidator = userValidator;
        this.userRepository = userRepository;
    }

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        User user = userRepository.findByUsername(username);
        userValidator.doesExist(username);
        return new org.springframework.security.core.userdetails.User(
                user.getUsername(),
                user.getPassword(),
                List.of(new SimpleGrantedAuthority("ROLE_" + user.getRole().name()))
        );
    }
}
